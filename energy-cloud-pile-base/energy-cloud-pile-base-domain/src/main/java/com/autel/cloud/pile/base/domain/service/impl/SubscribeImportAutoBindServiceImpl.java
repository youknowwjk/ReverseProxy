package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollectionUtil;
import com.alibaba.excel.EasyExcelFactory;
import com.alibaba.excel.util.StringUtils;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.util.DateUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.listener.BatchPageReadListener;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.service.SubscribeImportAutoBindCreateLicenceService;
import com.autel.cloud.pile.base.domain.service.SubscribeImportAutoBindService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.enums.FileExtensionEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.TbLenBindRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @description
 * @auther A23204
 * @datetime 2023/8/23 16:52
 */
@Service
@Slf4j
@RefreshScope
public class SubscribeImportAutoBindServiceImpl implements SubscribeImportAutoBindService {

    @Autowired
    private SubscribeImportAutoBindCreateLicenceService createLicenceService;

    @Autowired
    private OpLocationPileEvseRepository opLocationPileEvseRepository;

    @Autowired
    private TbLenBindRelationMapper tbLenBindRelationMapper;

    @Autowired
    private RedisUtil redisUtil;

    @Value("${subscribe.passFlag:true}")
    private Boolean passFlag;

    private static final List<String> YEAR_AREA = Arrays.asList("cdyl001", "cdyl006", "cdyp001", "cdyp006", "cms001", "cms006");

    // 商品ID
    // cdyl001	cdyp001	mp001	cms001
    // cdyl002	cdyp002	mp002	cms002
    // cdyl003	cdyp003	mp003	cms003
    // cdyl004	cdyp004	mp004	cms004
    // cdyl005	cdyp005	mp005	cms005
    // cdyl0056	cdyp006	mp006	cms006
    private static final List<String> GOODS_ID_LIST = Arrays.asList("cdyl001", "cdyp001", "cms001", "cdyl002", "cdyp002", "cms002", "cdyl003", "cdyp003", "cms003",
            "cdyl004", "cdyp004", "cms004", "cdyl005", "cdyp005", "cms005", "cdyl006", "cdyp006", "cms006");

    private static final List<String> GOODS_ID_LIST_ALL = Arrays.asList("cdyl001", "cdyp001", "cms001", "cdyl002", "cdyp002", "cms002", "cdyl003", "cdyp003", "cms003",
            "cdyl004", "cdyp004", "cms004", "cdyl005", "cdyp005", "cms005", "cdyl006", "cdyp006", "cms006","mp001","mp002","mp003","mp004","mp005","mp006");

    public static final String IMPORT_BIND_FAILED = "subs_import_bind_failed:";

    /**
     * description: startImport  商家桩导入
     * version: 1.0
     * date: 2023/8/23 16:55
     * author: A23204
     *
     * @param multipartFile
     * @return com.autel.cloud.base.http.pojo.Result<java.util.Map < java.lang.String, java.util.List < java.lang.String>>>
     */
    @Override
    public Result<SubscribeImportResp> startImport(MultipartFile multipartFile) {
        if (!passFlag) {
            // 开关拦截
            log.info("--->>> startImport,  passFlag is false.");
            return Result.ofFailed(HttpCodeEnum.METHOD_NOT_ALLOWED);
        }
        SubscribeImportResp subscribeImportResp = new SubscribeImportResp();
        List<SubscribeImportExists> exists = new ArrayList<>();
        List<SubscribeImportFailed> failed = new ArrayList<>();
        subscribeImportResp.setExists(exists);
        subscribeImportResp.setFailed(failed);

        String originalFilename = multipartFile.getOriginalFilename();
        log.info("newImport,originalFilename:{}", originalFilename);
        //校验文件后缀
        String extension = Objects.requireNonNull(FilenameUtils.getExtension(multipartFile.getOriginalFilename())).toLowerCase();
        if (!Objects.equals(extension, FileExtensionEnum.XLSX.getName())
                && !Objects.equals(extension, FileExtensionEnum.XLS.getName())
                && !Objects.equals(extension, FileExtensionEnum.CSV.getName())) {
            throw new MessageCodeException(PileBaseEnum.FILE_EXTENSION_WRONG);
        }

        // 文件格式校验成功

        List<HistorySellerPileImportDto> pileImportList = Collections.synchronizedList(new ArrayList<>());
        File file = multipartFile2File(multipartFile);

        EasyExcelFactory.read(file, HistorySellerPileImportDto.class, new BatchPageReadListener<HistorySellerPileImportDto>(pileImportList::addAll))
                .headRowNumber(1).sheet().autoTrim(true).doRead();

        log.info("originalCellValues:{}", JSON.toJSONString(pileImportList));

        // 检查校验非空字段。
        for (HistorySellerPileImportDto item : pileImportList) {
            if (StringUtils.isBlank(item.getSellerId()) || StringUtils.isBlank(item.getChargeType()) || StringUtils.isBlank(item.getPileSn())
                    || item.getGunCount() == null || StringUtils.isBlank(item.getStartTime()) || item.getServiceYear() == null
                    || StringUtils.isBlank(item.getGoodsId())) {
                // 补充完整数据
                log.info("--->>> please confirm data integrity, row:{}", JSON.toJSONString(item));
                return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, "文件数据检查失败，请确认该行数据完整性后重试:" + JSON.toJSONString(item));
            }
            // 商品ID	cdyl001	cdyp001	mp001	cms001			cdyl002	cdyp002	mp002	cms002				cdyl003	cdyp003	mp003	cms003				cdyl004	cdyp004	mp004	cms004				cdyl005	cdyp005	mp005	cms005				cdyl0056	cdyp006	mp006	cms006
            // 检查商品id是否正确
            String goodsId = item.getGoodsId();
            String[] split = goodsId.split("&");
            for (String goods : split) {
                if (!GOODS_ID_LIST.contains(goods)) {
                    log.info("--->>> please confirm data integrity, goodsId wrong, row:{}", JSON.toJSONString(item));
                    return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, "文件数据检查失败，当前行套餐id不正确，请修改后重试:" + JSON.toJSONString(item));
                }
            }

            // 桩类型检查
            if (!"AC".equals(item.getChargeType()) && !"DC".equals(item.getChargeType())) {
                log.info("--->>> please confirm data integrity, chargeType wrong, row:{}", JSON.toJSONString(item));
                return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, "文件数据检查失败，当前行桩类型不正确，请修改后重试:" + JSON.toJSONString(item));
            }

            // 时间格式检查
            try {
                String startTime = item.getStartTime();
                DateUtil.parseTimestampForDate(startTime);
            } catch (Exception e) {
                log.info("--->>> please confirm data integrity, startTime wrong, row:{}", JSON.toJSONString(item));
                return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, "文件数据检查失败，当前行生效时间格式不正确，正确格式：yyyy-MM-dd，请修改后重试:" + JSON.toJSONString(item));
            }

        }

        log.info("--->>> startImport, file check successful.");

        // 开始执行绑定逻辑
        pileImportList.forEach(pile -> {
            try {
                // 幂等性检查，根据 商家id + SKU + sn
                LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
                queryWrapper.eq(TbLenBindRelationEntity::getTenantId, pile.getSellerId().trim());
                List<String> goodsList = Arrays.asList(pile.getGoodsId().split("&"));
                queryWrapper.in(TbLenBindRelationEntity::getGoodsId, goodsList);
                queryWrapper.eq(TbLenBindRelationEntity::getPileSn, pile.getPileSn().trim());

                List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
                if (!CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
                    log.info("--->>> startImport, sellerId:{}, pile:{}, goodsId:{}, has been bound.", pile.getSellerId(), pile.getPileSn(), pile.getGoodsId());
                    SubscribeImportExists existItem = new SubscribeImportExists();
                    existItem.setSellerId(pile.getSellerId());
                    existItem.setPileSn(pile.getPileSn());
                    existItem.setGoodsId(pile.getGoodsId());
                    exists.add(existItem);
                    return;
                }
                // 尚未绑定， 解析sku，确定订阅周期、商品id，商品name， 订阅时长，权益id。
                /*
                *北美、澳洲订阅单位是年： cdyp001，  cdyp006
                *其他订阅单位都是月： cdyp002，cdyp003， cdyp004， cdyp005
                *
                * pro:
                *   goodsid:    cdyp001,    cdyp002,    cdyp003,    cdyp004
                *   name:       Pro
                *   unit:       YEAR(cdyp001, cdyp006),     MONTH:(cdyp002，cdyp003， cdyp004， cdyp005)
                *   sku:        cdyp001ac100001    cdyp001dc100001
                *   type:       GUN
                *
                * advising:
                *   goodsId:    cms001, cms002, cms003, cms004, cms005, cms006
                *   name:       Advertising
                *   unit:       YEAR(cdyp001, cdyp006),     MONTH:(cdyp002，cdyp003， cdyp004， cdyp005)
                *   sku:        cms001100001,cms001300001, cms001500001
                *   type:       PILE
                * */

                // 根据计量维度， 生成 entity，PILE、GUN。
                // 套餐也可能有2个。 ok
                // 注意异常处理，以 entity 维度处理。
                List<TbLenBindRelationEntity> entityList = tbLenBindRelationEntityBuild(pile);
                createLicenceService.saveLicence(entityList);

                List<String> pileList = entityList.stream().map(TbLenBindRelationEntity::getPileSn).collect(Collectors.toList());
                sendESNotice(pileList);

            } catch (Exception e) {
                log.error("--->>>startImport error, current sellerId:{}, pile:{}", pile.getSellerId(), pile.getPileSn(), e);
                SubscribeImportFailed failedItem = new SubscribeImportFailed();
                failedItem.setSellerId(pile.getSellerId());
                failedItem.setPileSn(pile.getPileSn());
                failedItem.setGoodsId(pile.getGoodsId());
                failed.add(failedItem);
            }
        });

        //
        redisUtil.set(IMPORT_BIND_FAILED, JSON.toJSONString(failed), 3600*24*7);
        return Result.ofSucceed(subscribeImportResp);
    }

    private List<TbLenBindRelationEntity> tbLenBindRelationEntityBuild(HistorySellerPileImportDto pile) {
        List<TbLenBindRelationEntity> list = new ArrayList<>();
        String goodsIdString = pile.getGoodsId();
        String[] split = goodsIdString.split("&");
        for (String goods : split) {
            int currentLoop = 1;
            if (goods.startsWith("cdyp") || goods.startsWith("cdyl")) {
                // 枪维度
                currentLoop = pile.getGunCount();
            }

            // loop currentLoop times
            for (int i = 0; i < currentLoop; i++) {
                TbLenBindRelationEntity bindRelationEntity = new TbLenBindRelationEntity();

                // 判断当前商品是枪维度、还是桩维度
                if (goods.startsWith("cdyp")) {
                    // pro套餐, 构建pro套餐的 TbLenBindRelationEntity
                    bindRelationEntity.setServiceId("biz:ops_official");
                    bindRelationEntity.setGoodsName("Pro");
                    bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_GUN);
                } else if (goods.startsWith("cdyl")) {
                    // lite 套餐
                    bindRelationEntity.setServiceId("biz:ops_official");
                    bindRelationEntity.setGoodsName("Lite");
                    bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_GUN);
                }else if (goods.startsWith("cms")) {
                    // advising 构建advising 套餐的 TbLenBindRelationEntity
                    bindRelationEntity.setServiceId("ads_official");
                    bindRelationEntity.setGoodsName("Advertising");
                    bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_PILE);
                } else {
                    // 未知套餐
                    log.info("--->>> current goodsId:{}, is not pro, lite or advising.", goods);
                    continue;
                }

                bindRelationEntity.setTenantId(pile.getSellerId().trim());
                bindRelationEntity.setOrderId(IdWorker.getIdStr());
                bindRelationEntity.setAgreementId(IdWorker.getIdStr());
                bindRelationEntity.setCreateTime(System.currentTimeMillis());
                bindRelationEntity.setUpdateTime(bindRelationEntity.getCreateTime());
                bindRelationEntity.setCreateBy("admin_import");
                bindRelationEntity.setUpdateBy("admin_import");
                bindRelationEntity.setPileSn(pile.getPileSn().trim());
                bindRelationEntity.setLicenceCode(RandomStringUtils.randomAlphabetic(16));
                bindRelationEntity.setChargeType(pile.getChargeType().trim());
                bindRelationEntity.setGoodsId(goods.trim());
                bindRelationEntity.setStatus(1);

                // 显示年
                if (YEAR_AREA.contains(goods.trim())) {
                    bindRelationEntity.setServiceTime(pile.getServiceYear());
                    bindRelationEntity.setTimeUnit("YEAR");
                } else {
                    bindRelationEntity.setServiceTime(pile.getServiceYear() * 12);
                    bindRelationEntity.setTimeUnit("MONTH");
                }

                Long available = DateUtil.parseTimestampForDate(pile.getStartTime());
                bindRelationEntity.setAvailableTime(available);

                Calendar c = Calendar.getInstance();
                c.setTimeInMillis(available);
                c.add(Calendar.YEAR, pile.getServiceYear());
                // 统一切齐到 23:59:59:999
                c.set(Calendar.MILLISECOND,999);
                c.set(Calendar.SECOND,59); //这是将当天的【秒】设置为59
                c.set(Calendar.MINUTE,59); //这是将当天的【分】设置为59
                c.set(Calendar.HOUR_OF_DAY,23); //这是将当天的【时】设置为23
                bindRelationEntity.setUnavailableTime(c.getTimeInMillis());

                /*
                 *  SKU 生成规则： goodsId + ac/dc + 100001
                 *  cdyl001ac100001
                 * */
                if (pile.getServiceYear() == 2) {
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "200001");
                } else if (pile.getServiceYear() == 3) {
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "300001");
                } else if (pile.getServiceYear() == 4) {
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "400001");
                } else if (pile.getServiceYear() == 5) {
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "500001");
                } else {
                    // default 1年
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "100001");
                }
                list.add(bindRelationEntity);
            }

        }
        return list;
    }

    /**
     * description: queryImportFailedSns 查询导入失败的桩信息 从缓存读取
     * version: 1.0
     * date: 2023/8/23 16:58
     * author: A23204
     *
     * @return com.autel.cloud.base.http.pojo.Result<java.util.Map < java.lang.String, java.util.List < java.lang.String>>>
     */
    @Override
    public Result<List<SubscribeImportFailed>> queryImportFailedSns() {
        String jsonString = (String) redisUtil.get(IMPORT_BIND_FAILED);
        log.info("--->>> queryImportFailedSns, jsonString:{}", jsonString);
        try {
            List<SubscribeImportFailed> list = JSON.parseArray(jsonString, SubscribeImportFailed.class);
            return Result.ofSucceed(list);
        } catch (Exception e) {
            log.error("--->>> queryImportFailedSns, parse json error", e);
            return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, jsonString);
        }
    }

    /**
     * description: bindLicenceManual 手动绑定下PO单商家的桩
     * version: 1.0
     * date: 2023/8/28 8:55
     * author: A23204
     *
     * @param subscribeBindManualReq
     * @return 返回更新失败的dto
     */
    @Override
    public Result<List<SubscribeBindManualDto>> bindLicenceManual(SubscribeBindManualReq subscribeBindManualReq) {
        if (!passFlag) {
            // 开关拦截
            log.info("--->>> bindLicenceManual,  passFlag is false.");
            return Result.ofFailed(HttpCodeEnum.METHOD_NOT_ALLOWED);
        }

        List<SubscribeBindManualDto> resp = new ArrayList<>();
        List<SubscribeBindManualDto> bindManualDtoList = subscribeBindManualReq.getBindManualDtoList();
        bindManualDtoList.forEach(subscribeBindManualDto -> {
            try {
                String startTime = subscribeBindManualDto.getStartTime();
                Long available = DateUtil.parseTimestampForDate(startTime);

                // 查询
                LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
                queryWrapper.eq(TbLenBindRelationEntity::getLicenceCode, subscribeBindManualDto.getLicenceCode());
                List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
                if (CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
                    resp.add(subscribeBindManualDto);
                    return;
                }

                TbLenBindRelationEntity tbLenBindRelationEntity = tbLenBindRelationEntities.get(0);
                int unit = tbLenBindRelationEntity.getTimeUnit().toLowerCase().startsWith("year") ? Calendar.YEAR : tbLenBindRelationEntity.getTimeUnit().toLowerCase().startsWith("month") ? Calendar.MONTH : Calendar.DAY_OF_YEAR;
                Calendar c = Calendar.getInstance();
                c.setTimeInMillis(available);
                c.add(unit, tbLenBindRelationEntity.getServiceTime());
                // 统一切齐到 23:59:59:999
                c.set(Calendar.MILLISECOND,999);
                c.set(Calendar.SECOND,59); //这是将当天的【秒】设置为59
                c.set(Calendar.MINUTE,59); //这是将当天的【分】设置为59
                c.set(Calendar.HOUR_OF_DAY,23); //这是将当天的【时】设置为23
                Long unavailable = c.getTimeInMillis();

                TbLenBindRelationEntity updateEntity = new TbLenBindRelationEntity();
                updateEntity.setId(tbLenBindRelationEntity.getId());
                updateEntity.setPileSn(subscribeBindManualDto.getPileSn());
                updateEntity.setAvailableTime(available);
                updateEntity.setUnavailableTime(unavailable);
                updateEntity.setStatus(1);
                updateEntity.setUpdateBy("admin_manual");
                updateEntity.setUpdateTime(System.currentTimeMillis());

                tbLenBindRelationMapper.updateById(updateEntity);
            } catch (Exception e) {
                log.error("bindLicenceManual, current dto:{} update failed.", JSON.toJSONString(subscribeBindManualDto));
                resp.add(subscribeBindManualDto);
            }
        });

        List<SubscribeBindManualDto> subscribeBindManualDtos = CollectionUtil.subtractToList(bindManualDtoList, resp);
        List<String> pileList = subscribeBindManualDtos.stream().map(SubscribeBindManualDto::getPileSn).collect(Collectors.toList());
        sendESNotice(pileList);

        return Result.ofSucceed(resp);
    }

    private File multipartFile2File(MultipartFile multipartFile) {
        File file = null;
        try {
            //获得原始文件名
            String fileName = multipartFile.getOriginalFilename();
            assert fileName != null;
            file = File.createTempFile(fileName.substring(0, fileName.lastIndexOf(".")), fileName.substring(fileName.lastIndexOf(".")));
            multipartFile.transferTo(file);
        } catch (IOException e) {
            log.error("multipartFile2File:", e);
        }
        return file;
    }

    /**
     * description: sendESNotice  更新ES，用户app地图展示
     * version: 1.0
     * date: 2023/8/28 16:49 
     * author: A23204 
     * 
     * @param pileList
     * @return void
     */ 
    private void sendESNotice(List<String> pileList) {
        try {
            // 发送绑定事件通知 ES
            UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO = new UpdateEsSubscriptionStatusDTO();
            updateEsSubscriptionStatusDTO.setPileSnList(pileList);
            updateEsSubscriptionStatusDTO.setStatus(Boolean.TRUE);
            opLocationPileEvseRepository.updateEsSubscriptionStatusByPileSnList(updateEsSubscriptionStatusDTO);
            log.info("--->>> sendESNotice success.");
        } catch (Exception e) {
            log.error("SubscribeImportAutoBindService.sendESNotice error, pileList:{}", pileList , e);
        }
    }

    /**
     * description: defaultSellerLicenceImport  默认一个月的商家重新导入
     * version: 1.0
     * date: 2023/9/17 14:23
     * author: A23204
     *
     * @param multipartFile
     * @return com.autel.cloud.base.http.pojo.Result<com.autel.cloud.pile.base.dto.SubscribeImportResp>
     */
    @Override
    public Result<SubscribeImportResp> defaultSellerLicenceImport(MultipartFile multipartFile) {
        if (!passFlag) {
            // 开关拦截
            log.info("--->>> defaultSellerLicenceImport,  passFlag is false.");
            return Result.ofFailed(HttpCodeEnum.METHOD_NOT_ALLOWED);
        }

        SubscribeImportResp subscribeImportResp = new SubscribeImportResp();
        List<SubscribeImportExists> exists = new ArrayList<>();
        List<SubscribeImportFailed> failed = new ArrayList<>();
        subscribeImportResp.setExists(exists);
        subscribeImportResp.setFailed(failed);

        String originalFilename = multipartFile.getOriginalFilename();

        log.info("--->>> defaultSellerLicenceImport,originalFilename:{}", originalFilename);

        //校验文件后缀
        String extension = Objects.requireNonNull(FilenameUtils.getExtension(multipartFile.getOriginalFilename())).toLowerCase();
        if (!Objects.equals(extension, FileExtensionEnum.XLSX.getName())
                && !Objects.equals(extension, FileExtensionEnum.XLS.getName())
                && !Objects.equals(extension, FileExtensionEnum.CSV.getName())) {
            throw new MessageCodeException(PileBaseEnum.FILE_EXTENSION_WRONG);
        }

        // 文件格式校验成功

        List<HistorySellerPileImportDto> pileImportList = Collections.synchronizedList(new ArrayList<>());
        File file = multipartFile2File(multipartFile);

        EasyExcelFactory.read(file, HistorySellerPileImportDto.class, new BatchPageReadListener<HistorySellerPileImportDto>(pileImportList::addAll))
                .headRowNumber(1).sheet().autoTrim(true).doRead();

        log.info("originalCellValues:{}", JSON.toJSONString(pileImportList));

        // 检查校验非空字段。
        for (HistorySellerPileImportDto item : pileImportList) {
            if (StringUtils.isBlank(item.getSellerId()) || StringUtils.isBlank(item.getChargeType()) || StringUtils.isBlank(item.getPileSn())
                    || item.getGunCount() == null || StringUtils.isBlank(item.getStartTime()) || item.getServiceYear() == null
                    || StringUtils.isBlank(item.getGoodsId())) {
                // 补充完整数据
                log.info("--->>> please confirm data integrity, row:{}", JSON.toJSONString(item));
                return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, "文件数据检查失败，请确认该行数据完整性后重试:" + JSON.toJSONString(item));
            }
            // 商品ID	cdyl001	cdyp001	mp001	cms001			cdyl002	cdyp002	mp002	cms002				cdyl003	cdyp003	mp003	cms003				cdyl004	cdyp004	mp004	cms004				cdyl005	cdyp005	mp005	cms005				cdyl0056	cdyp006	mp006	cms006
            // 检查商品id是否正确
            String goodsId = item.getGoodsId();
            String[] split = goodsId.split("&");
            for (String goods : split) {
                if (!GOODS_ID_LIST_ALL.contains(goods)) {
                    log.info("--->>> please confirm data integrity, goodsId wrong, row:{}", JSON.toJSONString(item));
                    return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, "文件数据检查失败，当前行套餐id不正确，请修改后重试:" + JSON.toJSONString(item));
                }
            }

            // 桩类型检查
            if (!"AC".equals(item.getChargeType()) && !"DC".equals(item.getChargeType())) {
                log.info("--->>> please confirm data integrity, chargeType wrong, row:{}", JSON.toJSONString(item));
                return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, "文件数据检查失败，当前行桩类型不正确，请修改后重试:" + JSON.toJSONString(item));
            }

            // 时间格式检查
            try {
                String startTime = item.getStartTime();
                DateUtil.parseTimestampForDate(startTime);
            } catch (Exception e) {
                log.info("--->>> please confirm data integrity, startTime wrong, row:{}", JSON.toJSONString(item));
                return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, "文件数据检查失败，当前行生效时间格式不正确，正确格式：yyyy-MM-dd，请修改后重试:" + JSON.toJSONString(item));
            }

        }

        log.info("--->>> startImport, file check successful.");

        // 开始执行绑定逻辑
        pileImportList.forEach(pile -> {
            try {
                // 幂等性检查，根据 商家id + SKU + sn
                LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
                queryWrapper.eq(TbLenBindRelationEntity::getTenantId, pile.getSellerId().trim());
                List<String> goodsList = Arrays.asList(pile.getGoodsId().split("&"));
                queryWrapper.in(TbLenBindRelationEntity::getGoodsId, goodsList);
                queryWrapper.eq(TbLenBindRelationEntity::getPileSn, pile.getPileSn().trim());
                queryWrapper.ne(TbLenBindRelationEntity::getRemark, "程序初始化的数据");


                List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
                if (!CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
                    log.info("--->>> startImport, sellerId:{}, pile:{}, goodsId:{}, has been bound.", pile.getSellerId(), pile.getPileSn(), pile.getGoodsId());
                    SubscribeImportExists existItem = new SubscribeImportExists();
                    existItem.setSellerId(pile.getSellerId());
                    existItem.setPileSn(pile.getPileSn());
                    existItem.setGoodsId(pile.getGoodsId());
                    exists.add(existItem);
                    return;
                }
                // 尚未绑定， 解析sku，确定订阅周期、商品id，商品name， 订阅时长，权益id。
                /*
                 *北美、澳洲订阅单位是年： cdyp001，  cdyp006
                 *其他订阅单位都是月： cdyp002，cdyp003， cdyp004， cdyp005
                 *
                 * pro:
                 *   goodsid:    cdyp001,    cdyp002,    cdyp003,    cdyp004
                 *   name:       Pro
                 *   unit:       YEAR(cdyp001, cdyp006),     MONTH:(cdyp002，cdyp003， cdyp004， cdyp005)
                 *   sku:        cdyp001ac100001    cdyp001dc100001
                 *   type:       GUN
                 *
                 * advising:
                 *   goodsId:    cms001, cms002, cms003, cms004, cms005, cms006
                 *   name:       Advertising
                 *   unit:       YEAR(cdyp001, cdyp006),     MONTH:(cdyp002，cdyp003， cdyp004， cdyp005)
                 *   sku:        cms001100001,cms001300001, cms001500001
                 *   type:       PILE
                 * */

                // 根据计量维度， 生成 entity，PILE、GUN。
                // 套餐也可能有2个。 ok
                // 注意异常处理，以 entity 维度处理。
                List<TbLenBindRelationEntity> entityList = tbLenBindRelationEntityBuildDefault(pile);
                createLicenceService.saveLicence(entityList);

                List<String> pileList = entityList.stream().map(TbLenBindRelationEntity::getPileSn).collect(Collectors.toList());
                sendESNotice(pileList);

            } catch (Exception e) {
                log.error("--->>>startImport error, current sellerId:{}, pile:{}", pile.getSellerId(), pile.getPileSn(), e);
                SubscribeImportFailed failedItem = new SubscribeImportFailed();
                failedItem.setSellerId(pile.getSellerId());
                failedItem.setPileSn(pile.getPileSn());
                failedItem.setGoodsId(pile.getGoodsId());
                failed.add(failedItem);
            }
        });

        //
        redisUtil.set(IMPORT_BIND_FAILED, JSON.toJSONString(failed), 3600*24*7);
        return Result.ofSucceed(subscribeImportResp);

    }

    private List<TbLenBindRelationEntity> tbLenBindRelationEntityBuildDefault(HistorySellerPileImportDto pile) {
        List<TbLenBindRelationEntity> list = new ArrayList<>();
        String goodsIdString = pile.getGoodsId();
        String[] split = goodsIdString.split("&");
        for (String goods : split) {
            int currentLoop = 1;
            if (goods.startsWith("cdyp") || goods.startsWith("cdyl")) {
                // 枪维度
                currentLoop = pile.getGunCount();
            }

            // loop currentLoop times
            for (int i = 0; i < currentLoop; i++) {
                TbLenBindRelationEntity bindRelationEntity = new TbLenBindRelationEntity();

                // 判断当前商品是枪维度、还是桩维度
                if (goods.startsWith("cdyp")) {
                    // pro套餐, 构建pro套餐的 TbLenBindRelationEntity
                    bindRelationEntity.setServiceId("biz:ops_official");
                    bindRelationEntity.setGoodsName("Pro");
                    bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_GUN);
                } else if (goods.startsWith("cdyl")) {
                    // lite 套餐
                    bindRelationEntity.setServiceId("biz:ops_official");
                    bindRelationEntity.setGoodsName("Lite");
                    bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_GUN);
                } else if (goods.startsWith("cms")) {
                    // advising 构建advising 套餐的 TbLenBindRelationEntity
                    bindRelationEntity.setServiceId("ads_official");
                    bindRelationEntity.setGoodsName("Advertising");
                    bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_PILE);
                } else if (goods.startsWith("mp")) {
                    // advising 构建advising 套餐的 TbLenBindRelationEntity
                    bindRelationEntity.setServiceId("biz:ops_official");
                    bindRelationEntity.setGoodsName("Maintenance");
                    bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_PILE);
                } else {
                    // 未知套餐
                    log.info("--->>> current goodsId:{}, is not pro, lite or advising.", goods);
                    continue;
                }

                bindRelationEntity.setTenantId(pile.getSellerId().trim());
                bindRelationEntity.setOrderId(IdWorker.getIdStr());
                bindRelationEntity.setAgreementId(IdWorker.getIdStr());
                bindRelationEntity.setCreateTime(System.currentTimeMillis());
                bindRelationEntity.setUpdateTime(bindRelationEntity.getCreateTime());
                bindRelationEntity.setCreateBy("admin_import");
                bindRelationEntity.setUpdateBy("admin_import");
                bindRelationEntity.setPileSn(pile.getPileSn().trim());
                bindRelationEntity.setLicenceCode(RandomStringUtils.randomAlphabetic(16));
                bindRelationEntity.setChargeType(pile.getChargeType().trim());
                bindRelationEntity.setGoodsId(goods.trim());
                bindRelationEntity.setStatus(1);

                // 显示年
                if (YEAR_AREA.contains(goods.trim())) {
                    bindRelationEntity.setServiceTime(pile.getServiceYear());
                    bindRelationEntity.setTimeUnit("YEAR");
                } else {
                    bindRelationEntity.setServiceTime(pile.getServiceYear() * 12);
                    bindRelationEntity.setTimeUnit("MONTH");
                }

                Long available = DateUtil.parseTimestampForDate(pile.getStartTime());
                bindRelationEntity.setAvailableTime(available);

                Calendar c = Calendar.getInstance();
                c.setTimeInMillis(available);
                c.add(Calendar.YEAR, pile.getServiceYear());
                // 统一切齐到 23:59:59:999
                c.set(Calendar.MILLISECOND,999);
                c.set(Calendar.SECOND,59); //这是将当天的【秒】设置为59
                c.set(Calendar.MINUTE,59); //这是将当天的【分】设置为59
                c.set(Calendar.HOUR_OF_DAY,23); //这是将当天的【时】设置为23
                bindRelationEntity.setUnavailableTime(c.getTimeInMillis());

                /*
                 *  SKU 生成规则： goodsId + ac/dc + 100001
                 *  cdyl001ac100001
                 * */
                if (pile.getServiceYear() == 2) {
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "200001");
                } else if (pile.getServiceYear() == 3) {
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "300001");
                } else if (pile.getServiceYear() == 4) {
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "400001");
                } else if (pile.getServiceYear() == 5) {
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "500001");
                } else {
                    // default 1年
                    bindRelationEntity.setSkuCode(goods.trim() + pile.getChargeType().trim().toLowerCase() + "100001");
                }

                list.add(bindRelationEntity);
            }

        }
        return list;
    }

    /**
     * description: deleteDefaultOneMonth  deleteDefaultOneMonth
     * version: 1.0
     * date: 2023/9/18 9:21
     * author: A23204
     *
     * @param pileSnList
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */
    @Override
    public Result<Boolean> deleteDefaultOneMonth(List<String> pileSnList) {
        if (!passFlag) {
            // 开关拦截
            log.info("--->>> deleteDefaultOneMonth,  passFlag is false.");
            return Result.ofFailed(HttpCodeEnum.METHOD_NOT_ALLOWED);
        }

        LambdaUpdateWrapper<TbLenBindRelationEntity> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.in(TbLenBindRelationEntity::getPileSn, pileSnList);
        updateWrapper.eq(TbLenBindRelationEntity::getRemark, "程序初始化的数据");

        tbLenBindRelationMapper.delete(updateWrapper);
        log.info("--->>> deleteDefaultOneMonth successful");
        return Result.ofSucceed(Boolean.TRUE);
    }
}
