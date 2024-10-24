package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.pile.base.domain.repository.*;
import com.autel.cloud.pile.base.domain.service.RuleDetailService;
import com.autel.cloud.pile.base.domain.service.RuleLocationPileService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.RuleElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.RuleElasticDTO;
import com.autel.cloud.pile.base.infrastructure.exception.MessageSourceUtil;
import com.autel.cloud.pile.base.infrastructure.mapper.RuleMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.user.api.constant.Constant;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.MemberGroupVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @Author temp
 * @Date 2022/8/2 17:55
 */
@Slf4j
@Service
public class RuleImplRepository extends ServiceImpl<RuleMapper, RuleEntity> implements RuleRepository {
    @Autowired
    private RuleDetailService ruleDetailService;
    @Autowired
    private OpLocationPileVipConfigRepository opLocationPileVipConfigRepository;
    @Autowired
    private RuleLocationPileService ruleLocationPileService;
    @Autowired
    @Lazy
    private OpLocationRepository opLocationService;
    @Autowired
    private RuleMapper ruleMapper;
    @Autowired
    private PileUserFeign pileUserFeign;
    @Autowired
    @Lazy
    private OpLocationPileEvseRepository opLocationPileEvseService;
    @Autowired
    private RuleElastic ruleElastic;

    @Autowired
    private OpLocationEvseRepository opLocationEvseRepository;
    @Autowired
    private MessageSourceUtil messageSourceUtil;
    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;

    @Resource
    private OpLocationRepository opLocationRepository;

    @Override
    @Transactional
    public Result<String> addRule(RuleDTO ruleDTO) {
        log.info("addRule,input ruleDTO={}", JSON.toJSONString(ruleDTO));
        //数据校验
        // 规则名称格式是够正确
        if(checkStringForSpecialChar(ruleDTO.getName())){
            throw new MessageCodeException(PileBaseEnum.RULE_NAME_ERROR);
        }

        if (CollectionUtils.isEmpty(ruleDTO.getDetails()) || ruleDTO.getDetails().size() > 24 * 7) {
            throw new MessageCodeException(PileBaseEnum.RULE_DETAIL_EMPTY);
        }
        List<RuleDetailDTO> details = ruleDTO.getDetails();
        details.forEach(rule -> {
            if (CollectionUtils.isEmpty(rule.getDays()) || rule.getDays().size() > 7) {
                throw new MessageCodeException(PileBaseEnum.RULE_DAYS_EMPTY);
            }
            if (StringUtils.hasText(rule.getMobile()) && !CommonUtil.checkPhone(rule.getMobile())) {
                throw new MessageCodeException(PileBaseEnum.MOBILE_FORMAT_ERROR);
            }
            if (StringUtils.hasText(rule.getEmail()) && !CommonUtil.checkEmail(rule.getEmail())) {
                throw new MessageCodeException(PileBaseEnum.EMAIL_FORMAT_ERROR);
            }
        });
        int existCount = this.count(new LambdaQueryWrapper<RuleEntity>().eq(RuleEntity::getSellerId, ruleDTO.getSellerId()).eq(RuleEntity::getName, ruleDTO.getName()).eq(RuleEntity::getDeleted, 0));
        log.info("addRule,existCount={}", existCount);
        if (existCount > 0) throw new MessageCodeException(PileBaseEnum.RULE_NAME_DUPLICATE);
        RuleEntity entity = new RuleEntity();
        entity.setId(IdWorker.getId());
        entity.setName(ruleDTO.getName());
        entity.setSellerId(ruleDTO.getSellerId());
        entity.setCreateBy(ruleDTO.getUserId());
        entity.setMemberType(ruleDTO.getMemberType());
        entity.setRuleType(ruleDTO.getRuleType());
        log.info("addRule,entity={}", JSON.toJSONString(entity));
        boolean save = this.save(entity);
        log.info("addRule,save={}", save);
        if (!save) {
            throw new RuntimeException("insert rule fail.");
        }
        Long ruleId = entity.getId();
        List<RuleDetailEntity> ruleDetailEntities = new ArrayList<>();
        ruleDTO.getDetails().forEach(detail -> {
            RuleDetailEntity ruleDetailEntity = new RuleDetailEntity();
            BeanUtils.copyProperties(detail, ruleDetailEntity);
            ruleDetailEntity.setId(IdWorker.getId());
            ruleDetailEntity.setRuleId(ruleId);
            ruleDetailEntity.setDays(detail.getDays().stream().map(String::valueOf).collect(Collectors.joining(",")));
            ruleDetailEntity.setCreatTime(System.currentTimeMillis());
            ruleDetailEntity.setMemberGroupId(detail.getMemberGroupId().stream().map(String::valueOf).collect(Collectors.joining(",")));
            ruleDetailEntities.add(ruleDetailEntity);
        });
        log.info("addRule,ruleDetailEntities={}", JSON.toJSONString(ruleDetailEntities));
        boolean saveBatch = ruleDetailService.saveBatch(ruleDetailEntities);
        log.info("addRule,saveBatch={}", saveBatch);
        if (!saveBatch) {
            throw new RuntimeException("batch insert rule detail fail.");
        }
        //同步到ES
        syncRuleToES(entity, ruleDetailEntities);
        if (!ObjectUtils.isEmpty(ruleDTO.getAssociatedGunDTO())){
            AssociatedGunDTO associatedGunDTO = ruleDTO.getAssociatedGunDTO();
            associatedGunDTO.setRuleId(entity.getId());
            associatedGunDTO.setRuleName(entity.getName());
            this.associatedGun(associatedGunDTO);
        }
        return Result.ofSucceed(String.valueOf(ruleId));
    }

    /**
     * 检查字符串是否包含这些特殊字符，并且长度在2-200
     * @param str
     * @return
     */
    public static boolean checkStringForSpecialChar(String str) {
        if(str.length() < 2 || str.length() > 200){
            return true;
        }
        String specialChars = "#@^&*~`¿";
        for(int i = 0; i < str.length(); i++){
            char c = str.charAt(i);
            if(specialChars.indexOf(c) >= 0){
                return true;
            }
        }
        return false;
    }

    @Override
    public void syncRuleToES(RuleEntity entity, List<RuleDetailEntity> ruleDetailEntities) {
        RuleElasticDTO dto = new RuleElasticDTO();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setRuleType(entity.getRuleType());
        dto.setSellerId(entity.getSellerId());
        dto.setCreateBy(entity.getCreateBy());
        dto.setDetails(JSON.toJSONString(ruleDetailEntities));
        dto.setMemberType(entity.getMemberType());
        RuleElasticDTO resultDto = ruleElastic.save(dto);
        log.info("syncRuleToES,resultDto={}", JSON.toJSONString(resultDto));
    }

    @Override
    @Transactional
    public Result<Boolean> editRule(RuleDTO ruleDTO) {
        log.info("editRule,ruleDTO={}", JSON.toJSONString(ruleDTO));
        Long ruleId = ruleDTO.getId();
        Long userId = ruleDTO.getUserId();
        String name = ruleDTO.getName();
        Long sellerId = ruleDTO.getSellerId();
        Integer ruleType = Optional.ofNullable(ruleDTO.getRuleType()).orElse(0);
        //数据校验
        RuleEntity existEntity = this.getOne(new LambdaQueryWrapper<RuleEntity>()
                .select(RuleEntity::getId, RuleEntity::getName, RuleEntity::getMemberType, RuleEntity::getVersion, RuleEntity::getRuleType)
                .eq(RuleEntity::getId, ruleId)
                .eq(RuleEntity::getDeleted, 0));
        log.info("editRule,existEntity={}", JSON.toJSONString(existEntity));
        if (existEntity == null) throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);

        if (!name.equals(existEntity.getName()) || !existEntity.getRuleType().equals(ruleType)) {
            //名称重复校验
            LambdaQueryWrapper<RuleEntity> query = Wrappers.lambdaQuery();
            query.eq(RuleEntity::getSellerId, sellerId);
            query.eq(RuleEntity::getDeleted, 0);
            query.eq(RuleEntity::getName, name);
            query.ne(RuleEntity::getId, ruleId);
            int count = this.count(query);
            if (count > 0) {
                throw new MessageCodeException(PileBaseEnum.RULE_NAME_DUPLICATE);
            }
            RuleEntity updateEntity = new RuleEntity();
            updateEntity.setId(ruleId);
            updateEntity.setName(ruleDTO.getName());
            updateEntity.setUpdateBy(userId);
            updateEntity.setRuleType(ruleType);
            boolean updateRule = this.updateById(updateEntity);
            log.info("editRule,updateRule={}", updateRule);
            if (!updateRule) {
                throw new RuntimeException("update rule fail.");
            }
            //同步规则名至已关联规则枪
            boolean syncName = opLocationEvseRepository.syncRuleName(ruleId, ruleDTO.getName());
            log.info("editRule,syncName={}", syncName);
            //用于同步到ES
            existEntity.setName(ruleDTO.getName());
            existEntity.setUpdateBy(userId);
            existEntity.setRuleType(ruleType);
        }
        List<RuleDetailDTO> details = ruleDTO.getDetails();
        List<RuleDetailEntity> ruleDetailEntities = new ArrayList<>(details.size());
        //先删除再新增
        if (!CollectionUtils.isEmpty(details)) {
            boolean batchInsert = ruleDetailService.insertBatchByRuleId(ruleId, details);
            log.info("editRule,batchInsert={}", batchInsert);
            if (!batchInsert) {
                throw new RuntimeException("batch insert rule detail fail.");
            }
            //用于同步到ES
            details.forEach(r -> {
                RuleDetailEntity entity = new RuleDetailEntity();
                BeanUtils.copyProperties(r, entity);
                entity.setRuleId(ruleId);
                entity.setDays(r.getDays().stream().map(String::valueOf).collect(Collectors.joining(",")));
                entity.setMemberGroupId(r.getMemberGroupId().stream().map(String::valueOf).collect(Collectors.joining(",")));
                entity.setCreatTime(System.currentTimeMillis());
                ruleDetailEntities.add(entity);
            });
        }
        //同步到ES
        this.syncRuleToES(existEntity, ruleDetailEntities);
        return Result.ofSucceed(true);
    }

    @Override
    @Transactional
    public Result<Boolean> deleteRule(Long userId, Long ruleId) {
        log.info("deleteRule,userId={},ruleId={}", userId, ruleId);
        int existCount = this.count(new LambdaQueryWrapper<RuleEntity>().eq(RuleEntity::getId, ruleId));
        if (existCount == 0) throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        //查看规则是否关联桩
        int relateCont = ruleLocationPileService.count(new LambdaQueryWrapper<RuleLocationPileEntity>().eq(RuleLocationPileEntity::getRuleId, ruleId));
        log.info("deleteRule,relateCont={}", relateCont);
        if (relateCont > 0) {
            throw new MessageCodeException(PileBaseEnum.RELATE_COUNT_NOT_EMPTY);
        }
        //先删除规则，再删除规则明细
        boolean removeRule = this.removeById(ruleId);
        log.info("deleteRule,removeRule={}", removeRule);
        if (!removeRule) {
            throw new RuntimeException("remove rule fail.");
        }
        int deleteCount = ruleDetailService.getBaseMapper().delete(new LambdaQueryWrapper<RuleDetailEntity>().eq(RuleDetailEntity::getRuleId, ruleId));
        log.info("deleteRule,deleteCount={}", deleteCount);
        if (deleteCount == 0) {
            throw new RuntimeException("remove rule details fail.");
        }
        //同步到ES
        ruleElastic.deleteById(ruleId);
        return Result.ofSucceed(true);
    }

    @Override
    public Result<List<RuleSiteVO>> locationList(RuleSiteDTO ruleSiteDTO) {
        log.info("locationList,ruleSiteDTO={}", JSON.toJSONString(ruleSiteDTO));
        String keyword = ruleSiteDTO.getKeyword();
        List<RuleSiteVO> resultList = new ArrayList<>();
        //查询场站和桩
        List<LocationInfoDTO> locationInfoDTOList = opLocationService.getLocationByKeyword(keyword);
        log.info("locationList,locationInfoDTOList={}", JSON.toJSONString(locationInfoDTOList));
        if (CollectionUtils.isEmpty(locationInfoDTOList)) return Result.ofSucceed(resultList);
        //先按场站分组，再按规则分组
        Map<Long, List<LocationInfoDTO>> locationMap = locationInfoDTOList.stream().collect(Collectors.groupingBy(LocationInfoDTO::getLocationId));
        locationMap.forEach((locationId, pileList) -> {
            RuleSiteVO ruleSiteVO = new RuleSiteVO();
            ruleSiteVO.setLocationId(locationId);
            ruleSiteVO.setLocationName(pileList.stream().findFirst().get().getLocationName());
            List<RuleInfoVO> ruleList = new ArrayList<>();
            //无关联规则
            List<LocationInfoDTO> noRuleList = pileList.stream().filter(r -> r.getRuleId() == null).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(noRuleList)) {
                List<PileInfoVO> piles = noRuleList.stream().map(pileDto -> {
                    PileInfoVO infoVO = new PileInfoVO();
                    infoVO.setPileId(pileDto.getPileId());
                    infoVO.setPileSn(pileDto.getPileSn());
                    infoVO.setPileName(pileDto.getPileName());
                    return infoVO;
                }).collect(Collectors.toList());
                ruleList.add(RuleInfoVO.getNoRuleInstance(piles));
            }
            //有关联规则
            if (ruleSiteDTO.getNoRelate() != null && ruleSiteDTO.getNoRelate() == 0) {
                List<LocationInfoDTO> hasRuleList = pileList.stream().filter(r -> r.getRuleId() != null).collect(Collectors.toList());
                if (!CollectionUtils.isEmpty(hasRuleList)) {
                    Map<Long, List<LocationInfoDTO>> ruleMap = hasRuleList.stream().collect(Collectors.groupingBy(LocationInfoDTO::getRuleId));
                    ruleMap.forEach((ruleId, list) -> {
                        RuleInfoVO ruleInfoVO = new RuleInfoVO();
                        ruleInfoVO.setRuleId(ruleId);
                        ruleInfoVO.setRuleName(list.stream().findFirst().get().getRuleName());
                        List<PileInfoVO> piles = list.stream().map(pileDto -> {
                            PileInfoVO pileInfoVO = new PileInfoVO();
                            pileInfoVO.setPileId(pileDto.getPileId());
                            pileInfoVO.setPileSn(pileDto.getPileSn());
                            pileInfoVO.setPileName(pileDto.getPileName());
                            return pileInfoVO;
                        }).collect(Collectors.toList());
                        ruleInfoVO.setPileList(piles);
                        ruleList.add(ruleInfoVO);
                    });
                }
            }
            ruleSiteVO.setRuleList(ruleList);
            resultList.add(ruleSiteVO);
        });
        log.info("locationList,resultList={}", JSON.toJSONString(resultList));
        return Result.ofSucceed(resultList);
    }

    @Override
    public Result<List<RuleVO>> ruleList(RulePageDTO rulePageDTO) {
        log.info("ruleList,rulePageDTO={}", JSON.toJSONString(rulePageDTO));
        Long sellerId = rulePageDTO.getSellerId();
        //按当前商家查询规则，再查询关联场站和桩数量
        List<RuleVO> ruleList = ruleMapper.findRulesBySellerId(sellerId, rulePageDTO.getKeyword());
        log.info("ruleList,ruleList={}", JSON.toJSONString(ruleList));
        List<Long> locationIds = pileUserFeign.getLocationIds().getData();
        log.info("ruleList,locationIds={}", JSON.toJSONString(locationIds));
        List<SiteInfoVo> siteList = opLocationService.getSiteList(locationIds, null);
        log.info("ruleList,siteList={}", JSON.toJSONString(siteList));
        if (!CollectionUtils.isEmpty(siteList)) {
            List<Long> pileIds = new ArrayList<>();
            siteList.forEach(siteInfoVo -> {
                List<PileInfoVO> piles = siteInfoVo.getPiles();
                if (piles != null && !piles.isEmpty()) {
                    pileIds.addAll(piles.stream().map(PileInfoVO::getPileId).collect(Collectors.toList()));
                }
            });
            if (!pileIds.isEmpty()) {
                List<RuleVO> relateList = ruleMapper.findRelateCount(pileIds);
                log.info("ruleList,relateList={}", JSON.toJSONString(relateList));
                //统计关联
                if (!CollectionUtils.isEmpty(relateList)) {
                    List<RuleVO> list = relateList.stream().filter(r -> r.getId() != null).collect(Collectors.toList());
                    if (!CollectionUtils.isEmpty(list)) {
                        ruleList.forEach(ruleVO -> {
                            list.forEach(r -> {
                                if (ruleVO.getId().longValue() == r.getId().longValue()) {
                                    ruleVO.setSiteCount(r.getSiteCount());
                                    ruleVO.setPileCount(r.getPileCount());
                                }
                            });
                        });
                    }
                }
            }
        }
        List<RuleVO> resultList = new ArrayList<>();
        resultList.addAll(ruleList);
        log.info("ruleList,resultList={}", JSON.toJSONString(resultList));
        return Result.ofSucceed(resultList);
    }

    @Override
    public IPage<RuleVO> ruleListNew(SerachBindEvseDTO serachBindEvseDTO) {
        log.info("ruleListNew,rulePageDTO={}", JSON.toJSONString(serachBindEvseDTO));
        Long sellerId = LoginUserUtil.getSellerId();
        IPage<RuleVO> resultPage = new Page<>(serachBindEvseDTO.getPage(),serachBindEvseDTO.getPageSize());
        // 判断排序类型
        if(serachBindEvseDTO.getOrderType() == null || serachBindEvseDTO.getOrderType().isEmpty()){
            serachBindEvseDTO.setOrderType("ASC");
        }
        //按当前商家查询规则，再查询关联场站,桩数量,枪数量
        resultPage = ruleMapper.searchPage(resultPage,serachBindEvseDTO,sellerId);
        List<RuleVO> ruleList = resultPage.getRecords();
        log.info("ruleListNew,ruleList={}", JSON.toJSONString(ruleList));
//        List<Long> locationIds = pileUserFeign.getLocationIds().getData();
//        log.info("ruleListNew,locationIds={}", JSON.toJSONString(locationIds));
//        List<OpLocationEvseElasticDTO> evseElasticDTOS = opLocationEvseRepository.finEvseListByOplocationIdAndSellerId(locationIds,sellerId);
//        if (CollectionUtils.isEmpty(evseElasticDTOS)) {
//            return Result.ofSucceed(resultPage);
//        }
//        Set<String> evseSnList = evseElasticDTOS.stream().filter(m -> !ObjectUtils.isEmpty(m.getRuleId())).map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toSet());
//        log.info("ruleListNew,evseSnList={}",evseSnList);
        // if (!CollectionUtils.isEmpty(evseSnList)) {
        List<RuleVO> countList = ruleMapper.findCountByRuleIds(new ArrayList<>(),sellerId);
        List<RuleVO> list = countList.stream().filter(r -> r.getId() != null).collect(Collectors.toList());
        ruleList.forEach(ruleVO -> {
            list.forEach(r -> {
                if (ruleVO.getId().longValue() == r.getId().longValue()) {
                    if (!ObjectUtils.isEmpty(r.getSiteCount())) {
                        ruleVO.setSiteCount(r.getSiteCount());
                    }
                    if (!ObjectUtils.isEmpty(r.getSiteCount())) {
                        ruleVO.setPileCount(r.getPileCount());
                    }
                    if (!ObjectUtils.isEmpty(r.getSiteCount())) {
                        ruleVO.setEvseCount(r.getEvseCount());
                    }
                    ruleVO.setMemberType(r.getMemberType());
                }
            });
        });
        //}
        List<Long> groupIds = new ArrayList<>();
        for (RuleVO ruleVO : ruleList) {
            for (RuleDetailVO detail : ruleVO.getDetails()) {
                String tmp = detail.getMemberGroupId();
                if (StringUtils.hasText(tmp)) {
                    groupIds.addAll(Arrays.asList(tmp.split(",")).stream().map(Long::valueOf).collect(Collectors.toList()));
                }
            }
        }
        if (CollectionUtils.isEmpty(groupIds)) {
            log.info("ruleListNew,groupIds is empty.");
            return resultPage;
        }
        List<MemberGroupVO> memberGroupVOList = new ArrayList<>();

        List<MemberGroupVO> memberGroupByIds = pileUserFeign.findMemberGroupByIds(new HashSet<>(groupIds)).getData();
        if (!CollectionUtils.isEmpty(memberGroupByIds)) {
            memberGroupVOList.addAll(memberGroupByIds);
        }
        if (groupIds.contains(-1L)) {
            MemberGroupVO vo = new MemberGroupVO();
            vo.setId(-1L);
            vo.setName(messageSourceUtil.getMessage(Constant.ALL_MEMBERS_NAME));
            memberGroupVOList.add(vo);
        }
        if (CollectionUtils.isEmpty(memberGroupVOList)) {
            log.info("ruleListNew,memberGroupVOList is empty.");
            return resultPage;
        }
        Map<Long, String> mapToUse = memberGroupVOList.stream().collect(Collectors.toMap(MemberGroupVO::getId, MemberGroupVO::getName));
        ruleList.stream().forEach(e -> {
            List<RuleDetailVO> details = e.getDetails();
            if (CollectionUtils.isEmpty(details)) {
                return;
            }
            Set<Long> ids = new HashSet<>();
            List<String> names = new ArrayList<>();
            details.stream().forEach(d -> {
                String tmp = d.getMemberGroupId();
                if (!StringUtils.hasText(tmp)) {
                    return;
                }
                ids.addAll(Arrays.stream(tmp.split(",")).map(Long::valueOf).collect(Collectors.toList()));
            });
            if (CollectionUtils.isEmpty(ids)) {
                return;
            }
            ids.stream().forEach(id -> {
                String name = mapToUse.get(id);
                if (!StringUtils.hasText(name)) {
                    return;
                }
                names.add(name);
            });
            e.setGroupName(names);
        });
        log.info("ruleListNew,resultList={}", JSON.toJSONString(ruleList));
        return resultPage;
    }

    @Override
    public Result<IPage<SerachBindEvseVO>> serachBindEvse(SerachBindEvseDTO serachBindEvseDTO) {
        Long sellerId = LoginUserUtil.getSellerId();
        IPage<SerachBindEvseVO> page = new Page<>(serachBindEvseDTO.getPage(),serachBindEvseDTO.getPageSize());
        IPage<SerachBindEvseVO> evseVOIPage = ruleMapper.findEvseBySellerId(page,serachBindEvseDTO,sellerId);
        if (ObjectUtils.isEmpty(evseVOIPage) || CollectionUtils.isEmpty(evseVOIPage.getRecords())) {
            return Result.ofSucceed(evseVOIPage);
        }
        List<SerachBindEvseVO> list = evseVOIPage.getRecords();
        Map<Long, String> locationNameMap = new HashMap<>();
        Set<Long> locationSet = list.stream().filter(m -> !ObjectUtils.isEmpty(m.getLocationId())).map(SerachBindEvseVO::getLocationId).collect(Collectors.toSet());
        //权限场站
        List<Long> locationIds;
        if (LoginUserUtil.isSellerAdmin()) {
            locationIds = this.opLocationService.getLocationIdBySellerId(sellerId);
        } else {
            locationIds = this.pileUserFeign.getLocationIds().getData();
        }
        //查询场站名称
        List<OpLocationEntity> locationInfoInLocationList = opLocationService.findLocationInfoInLocationList(new ArrayList<>(locationSet));
        if (!CollectionUtils.isEmpty(locationInfoInLocationList)) {
            locationNameMap = locationInfoInLocationList.stream().filter(m -> StringUtils.hasText(m.getName())).collect(Collectors.toMap(OpLocationEntity::getId, OpLocationEntity::getName, (e1, e2) -> e1));
        }
        for (SerachBindEvseVO serachBindEvseVO : list) {
            Boolean havePermission = false;
            serachBindEvseVO.setIsAssociated(true);
            Long locationId = serachBindEvseVO.getLocationId();
            if (!CollectionUtils.isEmpty(locationNameMap)) {
                serachBindEvseVO.setLocationName(locationNameMap.get(locationId));
            }
            if (!CollectionUtils.isEmpty(locationIds) && locationIds.contains(locationId)) {
                havePermission = true;
            }
            serachBindEvseVO.setHavePermission(havePermission);
            Integer gunNo = CommonUtil.getGunNo(serachBindEvseVO.getEvseSn());
            serachBindEvseVO.setGunNumber(gunNo != null ? gunNo.toString() : null);
        }
        return Result.ofSucceed(evseVOIPage);
    }

    @Override
    public IPage<SerachBindEvseVO> serachAllEvse(SerachBindEvseDTO dto,Long sellerId) {
        IPage<SerachBindEvseVO> resultPage = opLocationEvseRepository.getEvseInformationByLocationIds(dto, sellerId);
        List<SerachBindEvseVO> resultList = resultPage.getRecords();
        if (CollectionUtils.isEmpty(resultList)) {
            log.info("serachAllEvse,resultList is empty.");
            return resultPage;
        }
        List<String> evseSnList = resultList.stream().map(SerachBindEvseVO::getEvseSn).collect(Collectors.toList());
        Long ruleId = dto.getRuleId();
        List<SerachBindEvseVO> bindVoList = this.ruleMapper.findListByEvseSn(evseSnList, sellerId);
        if (CollectionUtils.isEmpty(bindVoList)) {
            log.info("serachAllEvse,bindVoList is empty.");
            return resultPage;
        }
        List<String> bindEvseSnList = bindVoList.stream().map(SerachBindEvseVO::getEvseSn).collect(Collectors.toList());
        resultList.stream().forEach(e -> {
            if (bindEvseSnList.contains(e.getEvseSn())) {
                e.setIsAssociated(true);
            }
        });
        return resultPage;
    }

    @Override
    public List<RuleGroupVO> getRuleNameByGroupId(List<Long> groupIds, Long sellerId) {
        if (ObjectUtils.isEmpty(groupIds) || ObjectUtils.isEmpty(sellerId)) {
            return null;
        }
        List<RuleGroupVO> ruleGroupVOS = ruleMapper.getRuleNameByGroupId(groupIds.stream().map(String::valueOf).collect(Collectors.toList()), sellerId);
        if (CollectionUtils.isEmpty(ruleGroupVOS)) {
            return null;
        }
        return ruleGroupVOS;
    }

    @Override
    public Integer syncRuleToEvse() {
        return ruleLocationPileService.syncRuleToEvse();
    }

    @Override
    public Integer syncMemberGroupId() {
        LambdaQueryWrapper<RuleDetailEntity> query = Wrappers.lambdaQuery();
        query.select(RuleDetailEntity::getId, RuleDetailEntity::getRuleId, RuleDetailEntity::getMemberGroupId);
        query.last("WHERE FIND_IN_SET('0',member_group_id)");
        List<RuleDetailEntity> ruleDetailEntityList = this.ruleDetailService.list(query);
        if (CollectionUtils.isEmpty(ruleDetailEntityList)) {
            log.info("syncMemberGroupId,ruleDetailEntityList is empty.");
            return 0;
        }
        List<Long> ruleIds = ruleDetailEntityList.stream().map(RuleDetailEntity::getRuleId).distinct().collect(Collectors.toList());
        LambdaQueryWrapper<RuleEntity> queryRule = Wrappers.lambdaQuery();
        queryRule.select(RuleEntity::getId, RuleEntity::getSellerId);
        queryRule.in(RuleEntity::getId, ruleIds);
        List<RuleEntity> ruleEntityList = this.list(queryRule);
        if (CollectionUtils.isEmpty(ruleEntityList)) {
            log.info("syncMemberGroupId,ruleEntityList is empty.");
            return 0;
        }
        Map<Long, Long> ruleMap = ruleEntityList.stream().collect(Collectors.toMap(RuleEntity::getId, RuleEntity::getSellerId, (f, s) -> f));
        List<Long> sellerIds = ruleEntityList.stream().map(RuleEntity::getSellerId).distinct().collect(Collectors.toList());
        List<MemberGroupVO> memberGroupVoList = this.pileUserFeign.findDefaultList(sellerIds).getData();
        if (CollectionUtils.isEmpty(memberGroupVoList)) {
            log.info("syncMemberGroupId,memberGroupVoList is empty.");
            return 0;
        }
        Map<Long, Long> mapToUse = memberGroupVoList.stream().collect(Collectors.toMap(MemberGroupVO::getSellerId, MemberGroupVO::getId, (f, s) -> f));
        List<RuleDetailEntity> updateList = new ArrayList<>();
        ruleDetailEntityList.stream().forEach(e -> {
            Long ruleId = e.getRuleId();
            String tmp = e.getMemberGroupId();
            if (!StringUtils.hasText(tmp)) {
                return;
            }
            Long sellerId = ruleMap.get(ruleId);
            if (sellerId == null) {
                return;
            }
            Long memberGroupId = mapToUse.get(sellerId);
            if (memberGroupId == null) {
                return;
            }
            List<Long> tmpList = Arrays.stream(tmp.split(",")).map(Long::valueOf).collect(Collectors.toList());
            List<Long> memberGroupIds = new ArrayList<>();
            tmpList.forEach(id -> {
                if (id.equals(0L)) {
                    memberGroupIds.add(memberGroupId);
                } else {
                    memberGroupIds.add(id);
                }
            });
            e.setMemberGroupId(memberGroupIds.stream().map(String::valueOf).collect(Collectors.joining(",")));
            updateList.add(e);
        });
        if (CollectionUtils.isEmpty(updateList)) {
            log.info("syncMemberGroupId,updateList is empty.");
            return 0;
        }
        this.ruleDetailService.updateBatchById(updateList);
        return updateList.size();
    }

    @Override
    public List<Long> findUse() {
        List<Long> resultList = new ArrayList<>();
        List<Long> tmpIds = this.ruleDetailService.findUse();
        if (!CollectionUtils.isEmpty(tmpIds)) {
            resultList.addAll(tmpIds);
        }
        List<OpLocationPileVipConfigEntity> tmpList = this.opLocationPileVipConfigRepository.findUse();
        if (CollectionUtils.isEmpty(tmpList)) {
            return resultList;
        }
        List<Long> sellerIds = tmpList.stream().map(OpLocationPileVipConfigEntity::getId).distinct().collect(Collectors.toList());
        List<MemberGroupVO> memberGroupVoList = this.pileUserFeign.findDefaultList(sellerIds).getData();
        if (CollectionUtils.isEmpty(memberGroupVoList)) {
            return resultList;
        }
        List<Long> ids = memberGroupVoList.stream().map(MemberGroupVO::getId).collect(Collectors.toList());
        Map<Long, List<OpLocationPileVipConfigEntity>> tmpMap = tmpList.stream().collect(Collectors.groupingBy(OpLocationPileVipConfigEntity::getId));
        tmpMap.forEach((sellerId, list) -> {
            list.stream().forEach(e -> {
                if (ids.contains(e.getPrincipalId())) {
                    sellerIds.remove(sellerId);
                    return;
                }
            });
        });
        if (!CollectionUtils.isEmpty(sellerIds)) {
            resultList.addAll(sellerIds);
        }
        return resultList;
    }

    @Override
    public Result<IPage<PileDetailVO>> relateList(RuleSitePageDTO ruleSitePageDTO) {
        log.info("relateList,ruleSitePageDTO={}", JSON.toJSONString(ruleSitePageDTO));
        Long ruleId = ruleSitePageDTO.getRuleId();
        IPage<PileDetailVO> resultPage = new Page<>();
        //获取所属场站
        List<Long> locationIds = ruleSitePageDTO.getLocationIds();
        if (CollectionUtils.isEmpty(ruleSitePageDTO.getLocationIds())) {
            locationIds = pileUserFeign.getLocationIds().getData();
        }
        log.info("relateList,locationIds={}", JSON.toJSONString(locationIds));
        if (CollectionUtils.isEmpty(locationIds)) {
            return Result.ofSucceed(resultPage);
        }
        //先过滤场站，再按场站和关键字查询
        List<RuleLocationPileDTO> relateLocationIds = ruleMapper.findRuleByLocationIds(locationIds, null, ruleId);
        log.info("relateList,relateLocationIds={}", JSON.toJSONString(relateLocationIds));
        if (CollectionUtils.isEmpty(relateLocationIds)) {
            return Result.ofSucceed(resultPage);
        }
        List<Long> pileIds = relateLocationIds.stream().map(RuleLocationPileDTO::getPileId).distinct().collect(Collectors.toList());
        locationIds = relateLocationIds.stream().map(RuleLocationPileDTO::getLocationId).distinct().collect(Collectors.toList());
        ruleSitePageDTO.setLocationIds(locationIds);
        ruleSitePageDTO.setPileIds(pileIds);
        IPage<PileDetailVO> locationInfoDTOList = opLocationService.getPilePageByKeyword(ruleSitePageDTO);
        log.info("relateList,locationInfoDTOList={}", JSON.toJSONString(locationInfoDTOList));
        return Result.ofSucceed(locationInfoDTOList);
    }

    @Override
    public Result<List<SiteInfoVo>> getSiteList(SiteDTO siteDTO) {
        log.info("getSiteList,siteDTO={}", JSON.toJSONString(siteDTO));
        //获取所属场站
        List<Long> locationIds = pileUserFeign.getLocationIds().getData();
        log.info("getSiteList,locationIds={}", JSON.toJSONString(locationIds));
        if (CollectionUtils.isEmpty(locationIds)) {
            return Result.ofSucceed();
        }
        List<SiteInfoVo> resultList = opLocationService.getSiteList(locationIds, null);
        log.info("getSiteList,resultList={}", JSON.toJSONString(resultList));
        if (CollectionUtils.isEmpty(resultList)) {
            return Result.ofSucceed();
        }
        List<RuleLocationPileDTO> relateLocationIds = ruleMapper.findRuleByLocationIds(locationIds, null, null);
        log.info("getSiteList,relateLocationIds={}", JSON.toJSONString(relateLocationIds));
        //设置已关联
        if (!CollectionUtils.isEmpty(relateLocationIds)) {
            Map<Long, List<RuleLocationPileDTO>> relateMap = relateLocationIds.stream().collect(Collectors.groupingBy(RuleLocationPileDTO::getLocationId));
            Long ruleId = siteDTO.getRuleId();
            resultList.forEach(r -> {
                Long locationId = r.getLocationId();
                List<RuleLocationPileDTO> pileList = relateMap.get(locationId);
                if (!CollectionUtils.isEmpty(pileList)) {
                    List<PileInfoVO> piles = r.getPiles();
                    pileList.forEach(pile -> {
                        piles.forEach(p -> {
                            if (p.getPileId().longValue() == pile.getPileId().longValue()) {
                                p.setIsRelate(true);
                                if (ruleId.longValue() == pile.getRuleId().longValue()) {
                                    p.setRuleId(ruleId);
                                }
                            }
                        });
                    });
                }
            });
        }
        //排除非关联本规则
        if (!CollectionUtils.isEmpty(resultList)) {
            List<SiteInfoVo> filterResultList = new ArrayList<>();
            resultList.forEach(r -> {
                SiteInfoVo vo = new SiteInfoVo();
                vo.setLocationName(r.getLocationName());
                vo.setLocationId(r.getLocationId());

                List<PileInfoVO> pileInfoVOS = new ArrayList<>();
                List<PileInfoVO> piles = r.getPiles();
                if (!CollectionUtils.isEmpty(piles)) {
                    piles.forEach(p -> {
                        if (!p.getIsRelate() || p.getRuleId() != null) {
                            pileInfoVOS.add(p);
                        }
                    });
                }
                if (!pileInfoVOS.isEmpty()) {
                    vo.setPiles(pileInfoVOS);
                    filterResultList.add(vo);
                }
            });
            log.info("getSiteList,filterResultList={}", JSON.toJSONString(filterResultList));
            resultList = filterResultList;
        }
        log.info("getSiteList,last resultList={}", JSON.toJSONString(resultList));
        return Result.ofSucceed(resultList);
    }

    @Override
    @Transactional
    public Result<Boolean> relatePile(RelatePileDTO relatePileDTO) {
        log.info("relatePile,relatePileDTO={}", JSON.toJSONString(relatePileDTO));
        List<PileDTO> piles = relatePileDTO.getPiles();
        Long ruleId = relatePileDTO.getRuleId();
        if (CollectionUtils.isEmpty(piles)) {
            return Result.ofSucceed();
        }
        int existCount = ruleLocationPileService.count(new LambdaQueryWrapper<RuleLocationPileEntity>()
                .eq(RuleLocationPileEntity::getRuleId, ruleId)
                .in(RuleLocationPileEntity::getLocationId, piles.stream().map(PileDTO::getLocationId).collect(Collectors.toList()))
                .in(RuleLocationPileEntity::getPileId, piles.stream().map(PileDTO::getPileId).collect(Collectors.toList())));
        log.info("relatePile,existCount={}", existCount);
        if (existCount > 0) {
            throw new MessageCodeException(PileBaseEnum.DATA_ALREADY_EXIST);
        }
        boolean saveBatch = ruleLocationPileService.batchInsert(ruleId, piles);
        log.info("relatePile,saveBatch={}", saveBatch);
        if (!saveBatch) {
            throw new RuntimeException("batch relate pile fail");
        }
        //同步到ES
        RuleEntity entity = this.getById(ruleId);
        relatePileDTO.setRuleName(entity.getName());
        boolean update = opLocationPileEvseService.updateRuleId(relatePileDTO);
        log.info("relatePile,update={}", update);
        if (!update) {
            throw new RuntimeException("sync rule to piles fail");
        }
        return Result.ofSucceed(update);
    }

    @Override
    @Transactional
    public Result<Boolean> associatedGun(AssociatedGunDTO associatedGunDTO) {
        log.info("associatedGun,gunInformationDTO={}", JSON.toJSONString(associatedGunDTO));
        if (ObjectUtils.isEmpty(associatedGunDTO) || ObjectUtils.isEmpty(associatedGunDTO.getRuleId())) {
            return Result.ofSucceed(null);
        }
        List<GunInformationDTO> piles = associatedGunDTO.getPiles();
        Long ruleId = associatedGunDTO.getRuleId();
        if (CollectionUtils.isEmpty(piles)) {
            return Result.ofSucceed();
        }
        List<String> evseSnList = piles.stream().map(m -> {
            String evseSn = m.getSn() + "_" + m.getGunNumber();
            return evseSn;
        }).collect(Collectors.toList());
        log.info("associatedGun,evseSnList={}",evseSnList);

        int existCount = ruleLocationPileService.count(new LambdaQueryWrapper<RuleLocationPileEntity>()
                .eq(RuleLocationPileEntity::getRuleId, ruleId)
                .in(RuleLocationPileEntity::getEvseSn, evseSnList));
        log.info("associatedGun,associatedGunDTO={}", existCount);
        if (existCount > 0) {
            throw new MessageCodeException(PileBaseEnum.DATA_ALREADY_EXIST);
        }
        boolean saveBatch = ruleLocationPileService.batchInsertEvse(ruleId, piles);
        log.info("associatedGun,saveBatch={}", saveBatch);
        if (!saveBatch) {
            throw new RuntimeException("batch relate pile fail");
        }
        //同步到ES
        RuleEntity entity = this.getById(ruleId);
        associatedGunDTO.setRuleName(entity.getName());
        boolean update = opLocationEvseRepository.updateRuleId(associatedGunDTO);
        log.info("relatePile,update={}", update);
        if (!update) {
            throw new RuntimeException("sync rule to piles fail");
        }
        return Result.ofSucceed(update);
    }
    @Override
    @Transactional
    public Result<Boolean> removePile(RemovePileDTO removePileDTO) {
        log.info("removePile,removePileDTO={}", JSON.toJSONString(removePileDTO));
        Long ruleId = removePileDTO.getRuleId();
        List<PileDTO> piles = removePileDTO.getPiles();
        if (CollectionUtils.isEmpty(piles)) {
            return Result.ofSucceed();
        }
        int deleteCount = ruleLocationPileService.getBaseMapper().delete(new LambdaQueryWrapper<RuleLocationPileEntity>()
                .eq(RuleLocationPileEntity::getRuleId, ruleId).in(RuleLocationPileEntity::getPileId, piles.stream().map(PileDTO::getPileId).collect(Collectors.toList())));
        log.info("removePile,deleteCount={}", deleteCount);
        Long replaceRuleId = removePileDTO.getReplaceRuleId();
        if (replaceRuleId != null && replaceRuleId == -1L) {
            //同步到ES
            RelatePileDTO relatePileDTO = new RelatePileDTO();
            relatePileDTO.setRuleId(replaceRuleId);
            relatePileDTO.setRuleName(null);
            relatePileDTO.setPiles(piles);
            boolean update = opLocationPileEvseService.updateRuleId(relatePileDTO);
            log.info("removePile,delete update={}", update);
            return Result.ofSucceed(true);
        }
        if (replaceRuleId != null) {
            ruleId = replaceRuleId;
        }
        boolean batchInsert = ruleLocationPileService.batchInsert(ruleId, piles);
        log.info("removePile,batchInsert={}", batchInsert);
        //同步到ES
        String ruleName = this.findById(replaceRuleId).getName();
        RelatePileDTO relatePileDTO = new RelatePileDTO();
        relatePileDTO.setRuleId(ruleId);
        relatePileDTO.setRuleName(ruleName);
        relatePileDTO.setPiles(piles);
        boolean update = opLocationPileEvseService.updateRuleId(relatePileDTO);
        log.info("removePile,replace update={}", update);
        return Result.ofSucceed(batchInsert);
    }

    @Override
    @Transactional
    public Result<Boolean> removeEvse(RemoveGunDTO removeGunDTO) {
        log.info("removeEvse,removeGunDTO={}", JSON.toJSONString(removeGunDTO));
        Long ruleId = removeGunDTO.getRuleId();
        List<GunInformationDTO> piles = removeGunDTO.getPiles();
        if (CollectionUtils.isEmpty(piles)) {
            return Result.ofSucceed();
        }
        //权限校验
        List<String> pileSnList = piles.stream().map(GunInformationDTO::getSn).collect(Collectors.toList());
        List<Long> locationIds = this.pileUserFeign.getLocationIds().getData();
        if (CollectionUtils.isEmpty(locationIds)) {
            throw new MessageCodeException(PileBaseEnum.NO_DATA_ACCESS);
        }
        List<OpLocationPileEvseElasticDTO> pileDtoList = this.opLocationPileEvseService.findList(pileSnList);
        if (!CollectionUtils.isEmpty(pileDtoList)) {
            List<Long> tmpLocationIds = pileDtoList.stream().map(OpLocationPileEvseElasticDTO::getLocationId).distinct().collect(Collectors.toList());
            Long noPermission = tmpLocationIds.stream().filter(locationId -> !locationIds.contains(locationId)).findAny().orElse(null);
            if (noPermission != null) {
                throw new MessageCodeException(PileBaseEnum.NO_DATA_ACCESS);
            }
        }
        int deleteCount = ruleLocationPileService.getBaseMapper().delete(new LambdaQueryWrapper<RuleLocationPileEntity>()
                .eq(RuleLocationPileEntity::getRuleId, ruleId)
                .in(RuleLocationPileEntity::getEvseSn, piles.stream().map(m -> {
                    return m.getSn() + "_" + m.getGunNumber();
                }).collect(Collectors.toList())));
        log.info("removeEvse,deleteCount={}", deleteCount);
        Long replaceRuleId = removeGunDTO.getReplaceRuleId();
        if (replaceRuleId != null && replaceRuleId == -1L) {
            //同步到ES
            AssociatedGunDTO associatedGunDTO = new AssociatedGunDTO();
            associatedGunDTO.setRuleId(replaceRuleId);
            associatedGunDTO.setRuleName(null);
            associatedGunDTO.setPiles(piles);
            boolean update = opLocationEvseRepository.updateRuleId(associatedGunDTO);
            log.info("removeEvse,delete update={}", update);
            return Result.ofSucceed(true);
        }
        if (replaceRuleId != null) {
            ruleId = replaceRuleId;
        }
        boolean batchInsert = ruleLocationPileService.batchInsertEvse(ruleId, piles);
        log.info("removeEvse,batchInsert={}", batchInsert);
        //同步到ES
        String ruleName = this.findById(replaceRuleId).getName();
        AssociatedGunDTO associatedGunDTO = new AssociatedGunDTO();
        associatedGunDTO.setRuleId(ruleId);
        associatedGunDTO.setRuleName(ruleName);
        associatedGunDTO.setPiles(piles);
        boolean update = opLocationEvseRepository.updateRuleId(associatedGunDTO);
        log.info("removeEvse,replace update={}", update);
        return Result.ofSucceed(batchInsert);
    }

    @Override
    public Result<List<RuleVO>> getAllRule() {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        Long sellerId = payload.getSellerId();
        List<RuleEntity> ruleList = this.list(new LambdaQueryWrapper<RuleEntity>()
                .select(RuleEntity::getId, RuleEntity::getName, RuleEntity::getRuleType)
                .eq(RuleEntity::getSellerId, sellerId)
                .eq(RuleEntity::getDeleted, 0));
        List<RuleVO> resultList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(ruleList)) {
            ruleList.forEach(entity -> {
                RuleVO vo = new RuleVO();
                vo.setId(entity.getId());
                vo.setName(entity.getName());
                vo.setRuleType(entity.getRuleType());
                resultList.add(vo);
            });
        }
        return Result.ofSucceed(resultList);
    }

    @Override
    public Result<List<PileRuleVO>> getRulesByPileSn(PullRuleDTO pullRuleDTO) {
        log.info("getRulesByPileSn,pullRuleDTO={}", JSON.toJSONString(pullRuleDTO));
        String pileSn = pullRuleDTO.getPileSn();
        //根桩SN码查询桩详情
        OpLocationPileEvseElasticDTO pileDto = opLocationPileEvseService.findByPileSn(pileSn);
        log.info("getRulesByPileSn,pileDto={}", JSON.toJSONString(pileDto));
        if (pileDto == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        Long locationId = pileDto.getLocationId();
        List<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseService.findByLocationId(locationId);
        log.info("getRulesByPileSn,pileDtoList={}", JSON.toJSONString(pileDtoList));
        List<Long> ruleIds = pileDtoList.stream().filter(p -> p.getRuleId() != null).map(OpLocationPileEvseElasticDTO::getRuleId).distinct().collect(Collectors.toList());
        Map<Long, RuleVO> ruleVoMap = null;
        List<PileRuleVO> resultList = new ArrayList<>(pileDtoList.size());
        if (!CollectionUtils.isEmpty(ruleIds)) {
            List<RuleVO> ruleVOList = this.findAllByIds(ruleIds);
            if (!CollectionUtils.isEmpty(ruleVOList)) {
                ruleVoMap = ruleVOList.stream().collect(Collectors.toMap(RuleVO::getId, r -> r));
            }
        }
        Map<Long, RuleVO> finalRuleVoMap = ruleVoMap;
        pileDtoList.forEach(pile -> {
            PileRuleVO vo = new PileRuleVO();
            vo.setPileId(pile.getId());
            vo.setPileName(pile.getName());
            vo.setPileSn(pile.getPileSn());
            if (!CollectionUtils.isEmpty(finalRuleVoMap) && pile.getRuleId() != null) {
                vo.setRuleVO(finalRuleVoMap.get(pile.getRuleId()));
            }
            resultList.add(vo);
        });
        log.info("getRulesByPileSn,resultList={}", JSON.toJSONString(resultList));
        return Result.ofSucceed(resultList);
    }

    @Override
    public Result<List<PileRuleVO>> getRulesByEvseSn(GetRuleByEvseSnDTO getRuleByEvseSnDTO) {
        log.info("getRulesByEvseSn,getRuleByEvseSnDTO={}", JSON.toJSONString(getRuleByEvseSnDTO));
        String evseSn = getRuleByEvseSnDTO.getEvseSn();
        //根桩SN码查询桩详情
        OpLocationEvseElasticDTO byEvseSn = opLocationEvseRepository.findByEvseSn(evseSn);
        log.info("getRulesByEvseSn,byEvseSn={}", JSON.toJSONString(byEvseSn));
        if (byEvseSn == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        Long locationId = byEvseSn.getLocationId();
        List<OpLocationEvseElasticDTO> evseList = opLocationEvseRepository.findByLocationId(locationId);
        log.info("getRulesByEvseSn,evseList={}", JSON.toJSONString(evseList));
        List<Long> ruleIds = evseList.stream().filter(p -> p.getRuleId() != null).map(OpLocationEvseElasticDTO::getRuleId).distinct().collect(Collectors.toList());
        Map<Long, RuleVO> ruleVoMap = null;
        List<PileRuleVO> resultList = new ArrayList<>(evseList.size());
        if (!CollectionUtils.isEmpty(ruleIds)) {
            List<RuleVO> ruleVOList = this.findAllByIds(ruleIds);
            if (!CollectionUtils.isEmpty(ruleVOList)) {
                ruleVoMap = ruleVOList.stream().collect(Collectors.toMap(RuleVO::getId, r -> r));
            }
        }
        Map<Long, RuleVO> finalRuleVoMap = ruleVoMap;
        evseList.forEach(evse -> {
            PileRuleVO vo = new PileRuleVO();
            vo.setPileId(evse.getId());
            vo.setPileName(evse.getPileName());
            vo.setPileSn(evse.getPileSn());
            vo.setEvseSn(evse.getEvseSn());
            if (!CollectionUtils.isEmpty(finalRuleVoMap) && evse.getRuleId() != null) {
                vo.setRuleVO(finalRuleVoMap.get(evse.getRuleId()));
            }
            resultList.add(vo);
        });
        log.info("getRulesByEvseSn,resultList={}", JSON.toJSONString(resultList));
        return Result.ofSucceed(resultList);
    }

    @Override
    public Result<List<PileRuleVO>> getRuleByLocationId(PullRuleDTO pullRuleDTO) {
        log.info("getRuleByLocationId,pullRuleDTO={}", JSON.toJSONString(pullRuleDTO));
        Long locationId = pullRuleDTO.getLocationId();
        //根据场站ID查询桩
        List<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseService.findByLocationId(locationId);
        log.info("getRuleByLocationId,pileDtoList={}", JSON.toJSONString(pileDtoList));
        if (CollectionUtils.isEmpty(pileDtoList)) {
            return Result.ofSucceed();
        }
        List<Long> ruleIds = pileDtoList.stream().map(OpLocationPileEvseElasticDTO::getRuleId).distinct().filter(Objects::nonNull).collect(Collectors.toList());
        List<RuleVO> ruleVOList = this.findAllByIds(ruleIds);
        List<PileRuleVO> resultList = new ArrayList<>(pileDtoList.size());
        log.info("getRuleByLocationId,ruleVOList={}", JSON.toJSONString(ruleVOList));
        pileDtoList.forEach(dto -> {
            PileRuleVO pileRuleVO = new PileRuleVO();
            pileRuleVO.setPileId(dto.getId());
            pileRuleVO.setPileName(dto.getName());
            if (!CollectionUtils.isEmpty(ruleVOList)) {
                ruleVOList.forEach(vo -> {
                    if (dto.getRuleId() != null && (dto.getRuleId().longValue() == vo.getId().longValue())) {
                        pileRuleVO.setRuleVO(vo);
                    }
                });
            }
            resultList.add(pileRuleVO);
        });
        log.info("getRuleByLocationId,resultList={}", JSON.toJSONString(resultList));
        return Result.ofSucceed(resultList);
    }

    @Override
    public Result<List<PileRuleVO>> getEvseRulesByLocationId(GetRuleByEvseSnDTO getRuleByEvseSnDTO) {
        log.info("getEvseRulesByLocationId,pullRuleDTO={}", JSON.toJSONString(getRuleByEvseSnDTO));
        Long locationId = getRuleByEvseSnDTO.getLocationId();
        //根据场站ID查询桩
        List<OpLocationEvseElasticDTO> evseElasticDTOS = opLocationEvseRepository.findByLocationId(locationId);
        log.info("getEvseRulesByLocationId,evseElasticDTOS={}", JSON.toJSONString(evseElasticDTOS));
        if (CollectionUtils.isEmpty(evseElasticDTOS)) {
            return Result.ofSucceed();
        }
        List<Long> ruleIds = evseElasticDTOS.stream().map(OpLocationEvseElasticDTO::getRuleId).distinct().filter(Objects::nonNull).collect(Collectors.toList());
        List<RuleVO> ruleVOList = this.findAllByIds(ruleIds);
        List<PileRuleVO> resultList = new ArrayList<>(evseElasticDTOS.size());
        log.info("getEvseRulesByLocationId,ruleVOList={}", JSON.toJSONString(ruleVOList));
        evseElasticDTOS.forEach(dto -> {
            PileRuleVO pileRuleVO = new PileRuleVO();
            pileRuleVO.setPileId(dto.getId());
            pileRuleVO.setPileName(dto.getPileName());
            if (!CollectionUtils.isEmpty(ruleVOList)) {
                ruleVOList.forEach(vo -> {
                    if (dto.getRuleId() != null && (dto.getRuleId().longValue() == vo.getId().longValue())) {
                        pileRuleVO.setRuleVO(vo);
                    }
                });
            }
            pileRuleVO.setEvseSn(dto.getEvseSn());
            pileRuleVO.setPileSn(dto.getPileSn());
            resultList.add(pileRuleVO);
        });
        log.info("getEvseRulesByLocationId,resultList={}", JSON.toJSONString(resultList));
        return Result.ofSucceed(resultList);
    }

    @Override
    public RuleVO findById(Long ruleId) {
        log.info("findById,ruleId={}", ruleId);
        Optional<RuleElasticDTO> ruleDto = ruleElastic.findById(ruleId);
        log.info("findById,es ruleDto={}", JSON.toJSONString(ruleDto));
        if (ruleDto.isPresent()) {
            RuleElasticDTO r = ruleDto.get();
            RuleVO vo = new RuleVO();
            vo.setId(r.getId());
            vo.setName(r.getName());
            String details = r.getDetails();
            if (StringUtils.hasText(details)) {
                List<RuleDetailVO> detailVOS = JSON.parseArray(details, RuleDetailVO.class);
                vo.setDetails(detailVOS);
            }
            vo.setMemberType(r.getMemberType());
            vo.setRuleType(r.getRuleType());
            return vo;
        }
        RuleVO entity = ruleMapper.findById(ruleId);
        log.info("findById,db entity={}", JSON.toJSONString(entity));
        return entity;
    }

    @Override
    public List<RuleVO> findAllByIds(List<Long> ruleIds) {
        log.info("findAllByIds,ruleIds={}", JSON.toJSONString(ruleIds));
        if (CollectionUtils.isEmpty(ruleIds)) {
            return null;
        }
        Iterable<RuleElasticDTO> ruleDtoList = ruleElastic.findAllById(ruleIds);
        log.info("findAllByIds,ruleDtoList={}", JSON.toJSONString(ruleDtoList));
        List<RuleVO> ruleVOList = new ArrayList<>();
        ruleDtoList.forEach(dto -> {
            RuleVO vo = new RuleVO();
            vo.setId(dto.getId());
            vo.setName(dto.getName());
            String details = dto.getDetails();
            if (StringUtils.hasText(details)) {
                List<RuleDetailVO> detailVOS = JSON.parseArray(details, RuleDetailVO.class);
                vo.setDetails(detailVOS);
            }
            vo.setRuleType(dto.getRuleType());
            ruleVOList.add(vo);
        });
        log.info("findAllByIds,ruleVOList={}", JSON.toJSONString(ruleVOList));
        return ruleVOList;
    }

    @Override
    public Result<List<RuleVO>> getAllRuleByLocationId(Long locationId) {
        log.info("getAllRuleByLocationId,locationId={}", locationId);
        //根据场站ID查询桩
        List<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseService.findByLocationId(locationId);
        log.info("getAllRuleByLocationId,pileDtoList={}", JSON.toJSONString(pileDtoList));
        if (CollectionUtils.isEmpty(pileDtoList)) {
            return Result.ofSucceed(new ArrayList<>());
        }
        List<Long> ruleIds = pileDtoList.stream().filter(p -> p.getRuleId() != null).map(OpLocationPileEvseElasticDTO::getRuleId).distinct().collect(Collectors.toList());
        List<RuleVO> ruleVOList = this.findAllByIds(ruleIds);
        log.info("getAllRuleByLocationId,ruleVOList={}", JSON.toJSONString(ruleVOList));
        return Result.ofSucceed(ruleVOList);
    }

    @Override
    public Result<RuleVO> detail(Long ruleId) {
        log.info("detail,ruleId={}", ruleId);
        RuleVO result = this.findById(ruleId);
        log.info("detail,result={}", JSON.toJSONString(result));
        return Result.ofSucceed(result);
    }

    @Override
    public Result<RuleRelateForAppVO> getRuleForApp(RuleRelateForAppDTO ruleRelateForAppDTO) {
        log.info("getRuleForApp,ruleRelateForAppDTO={}", ruleRelateForAppDTO);
        Long locationId = ruleRelateForAppDTO.getLocationId();
        Long sellerId = ruleRelateForAppDTO.getSellerId();
        Long userId = ruleRelateForAppDTO.getUserId();
        String timeZone = ruleRelateForAppDTO.getZoneId();
        ZoneId zoneId = ZoneId.of(timeZone);
        //根据场站ID查询枪
        List<OpLocationEvseElasticDTO> evseDtoList = this.opLocationEvseRepository.findByLocationId(locationId);
        log.info("getRuleForApp,evseDtoList={}", JSON.toJSONString(evseDtoList));
        if (CollectionUtils.isEmpty(evseDtoList)) {
            return Result.ofSucceed();
        }
        List<Long> ruleIds = evseDtoList.stream().map(OpLocationEvseElasticDTO::getRuleId).distinct().filter(Objects::nonNull).collect(Collectors.toList());
        List<RuleVO> ruleVOList = this.findAllByIds(ruleIds);
        log.info("getRuleForApp,ruleVOList={}", JSON.toJSONString(ruleVOList));
        RuleRelateForAppVO resultVo = new RuleRelateForAppVO();
        //只要有一个桩无规则关联，场站开放
        if (CollectionUtils.isEmpty(ruleVOList) || evseDtoList.stream().anyMatch(p -> p.getRuleId() == null)) {
            resultVo.setIsLimit(false);
            resultVo.setChangingTime("24:00");
            return Result.ofSucceed(resultVo);
        }
        //查询用户所属分组
        Result<List<Long>> groupIdResult = pileUserFeign.findGroupId(userId, sellerId);
        log.info("getRuleForApp,groupIdResult={}", groupIdResult);
        if (groupIdResult != null && groupIdResult.getCode() == 200 && groupIdResult.getData() != null) {
            //新用户组id=0
            List<Long> memberGroupIds = groupIdResult.getData();
            resultVo.setMemberGroupIds(memberGroupIds);
        }
        //是否进场限制
        this.setSiteLimit(resultVo, evseDtoList, ruleVOList, zoneId);
        //场站营业状态
        this.setChangingTime(resultVo, ruleVOList, zoneId);
        log.info("getRuleForApp,now={},resultVo={}", LocalDateTime.now(zoneId), JSON.toJSONString(resultVo));
        return Result.ofSucceed(resultVo);
    }

    @Override
    public Result<RuleVO> getRuleByPileSn(PullRuleDTO pullRuleDTO) {
        log.info("getRuleByPileSn,pullRuleDTO={}", JSON.toJSONString(pullRuleDTO));
        String pileSn = pullRuleDTO.getPileSn();
        //根桩SN码查询桩详情
        OpLocationPileEvseElasticDTO pileDto = opLocationPileEvseService.findByPileSn(pileSn);
        log.info("getRuleByPileSn,pileDto={}", JSON.toJSONString(pileDto));
        if (pileDto == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        if (pileDto.getRuleId() == null) {
            return Result.ofSucceed();
        }
        RuleVO ruleVO = this.findById(pileDto.getRuleId());
        log.info("getRuleByPileSn,ruleVO={}", JSON.toJSONString(ruleVO));
        return Result.ofSucceed(ruleVO);
    }

    @Override
    public RuleVO getRuleByEvseSn(GetRuleByEvseSnDTO getRuleByEvseSnDTO) {
        log.info("getRuleByEvseSn,getRuleByEvseSnDTO={}", JSON.toJSONString(getRuleByEvseSnDTO));
        String evseSn = getRuleByEvseSnDTO.getEvseSn();
        //根桩SN码查询桩详情
        OpLocationEvseElasticDTO evseDto = opLocationEvseRepository.findByEvseSn(evseSn);
        log.info("getRuleByEvseSn,evseDto={}", JSON.toJSONString(evseDto));
        if (evseDto == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        if (evseDto.getRuleId() == null) {
            log.info("getRuleByEvseSn,ruleId is null.");
            return null;
        }
        RuleVO ruleVO = this.findById(evseDto.getRuleId());
        log.info("getRuleByEvseSn,ruleVO={}", JSON.toJSONString(ruleVO));
        try {
            //基础服务需求，返回值增加客户组名称
            Set<Long> groupIds = new HashSet<>();
            for (RuleDetailVO detail : ruleVO.getDetails()) {
                String tmp = detail.getMemberGroupId();
                if (StringUtils.hasText(tmp)) {
                    List<Long> tmpIds = Arrays.stream(tmp.split(",")).map(Long::valueOf).collect(Collectors.toList());
                    groupIds.addAll(tmpIds);
                }
            }
            if (CollectionUtils.isEmpty(groupIds)) {
                log.info("getRuleByEvseSn,groupIds is empty.");
                return ruleVO;
            }
            List<MemberGroupVO> memberGroupVOList = new ArrayList<>();
            List<MemberGroupVO> memberGroupByIds = pileUserFeign.findMemberGroupByIds(groupIds).getData();
            if (!CollectionUtils.isEmpty(memberGroupByIds)) {
                memberGroupVOList.addAll(memberGroupByIds);
            }
            if (groupIds.contains(-1L)) {
                MemberGroupVO vo = new MemberGroupVO();
                vo.setId(-1L);
                vo.setName(messageSourceUtil.getMessage(Constant.ALL_MEMBERS_NAME));
                vo.setMemberType(0);
                memberGroupVOList.add(vo);
            }
            if (CollectionUtils.isEmpty(memberGroupVOList)) {
                log.info("getRuleByEvseSn,memberGroupVOList is empty.");
                return ruleVO;
            }
            List<RuleDetailVO> details = ruleVO.getDetails();
            if (CollectionUtils.isEmpty(details)) {
                log.info("getRuleByEvseSn,details is empty.");
                return ruleVO;
            }
            Map<Long, MemberGroupVO> mapToUse = memberGroupVOList.stream().collect(Collectors.toMap(MemberGroupVO::getId, e -> e, (f, s) -> f));
            details.stream().forEach(detail -> {
                List<String> groupNameList = new ArrayList<>();
                List<String> cardGroupNameList = new ArrayList<>();
                String tmp = detail.getMemberGroupId();
                if (!StringUtils.hasText(tmp)) {
                    return;
                }
                List<Long> tmpIds = Arrays.stream(tmp.split(",")).map(Long::valueOf).collect(Collectors.toList());
                tmpIds.stream().forEach(memberGroupId -> {
                    MemberGroupVO vo = mapToUse.get(memberGroupId);
                    if (vo == null) {
                        return;
                    }
                    // 客户分组
                    if (vo.getMemberType().equals(0)) {
                        groupNameList.add(vo.getName());
                    }
                    //充电卡分组
                    if (vo.getMemberType().equals(2)) {
                        cardGroupNameList.add(vo.getName());
                    }
                });
                detail.setCardGroupName(cardGroupNameList);
                detail.setMemberGroupName(groupNameList);
            });
        } catch (Exception e) {
            log.info("getRuleByEvseSn,e={}", e);
        }

        hourMinutes24Build(ruleVO);
        return ruleVO;
    }

    private void hourMinutes24Build(RuleVO ruleVO) {
        if (ruleVO != null && ruleVO.getDetails() != null) {
            List<RuleDetailVO> details = ruleVO.getDetails();
            if (CollUtil.isNotEmpty(details)) {
                details.forEach(ruleDetailVO -> {
                    ruleDetailVO.setStartTime24(ruleDetailVO.getStartTime());
                    ruleDetailVO.setEndTime24(ruleDetailVO.getEndTime());
                });
            }
        }
    }

    private void setChangingTime(RuleRelateForAppVO resultVo, List<RuleVO> ruleVOList, ZoneId zoneId) {
        LocalDateTime now = LocalDateTime.now(zoneId);
        int today = now.getDayOfWeek().getValue();
        LocalTime localTime = now.toLocalTime();
        ruleVOList.forEach(rule -> {
            List<RuleDetailVO> details = rule.getDetails();
            details.forEach(detail -> {
                List<Integer> dayList = Arrays.stream(detail.getDays().split(",")).map(Integer::valueOf).collect(Collectors.toList());
                if (dayList.contains(today)) {
                    String startTime = detail.getStartTime();
                    String endTime = detail.getEndTime();
                    //首次赋值
                    if (resultVo.getOpenStartTime() == null && resultVo.getOpenEndTime() == null) {
                        if ((localTime.compareTo(LocalTime.parse(startTime)) >= 0 && localTime.compareTo(LocalTime.parse(endTime)) <= 0)
                                || localTime.compareTo(LocalTime.parse(startTime)) < 0) {
                            resultVo.setOpenStartTime(startTime);
                            resultVo.setOpenEndTime(endTime);
                        }
                    } else {
                        if (localTime.compareTo(LocalTime.parse(startTime)) >= 0 && localTime.compareTo(LocalTime.parse(endTime)) <= 0) {
                            if (localTime.compareTo(LocalTime.parse(resultVo.getOpenStartTime())) < 0) {
                                if (LocalTime.parse(resultVo.getOpenStartTime()).compareTo(LocalTime.parse(endTime)) <= 0
                                        && LocalTime.parse(resultVo.getOpenEndTime()).compareTo(LocalTime.parse(endTime)) > 0) {
                                    resultVo.setOpenStartTime(startTime);
                                } else {
                                    resultVo.setOpenStartTime(startTime);
                                    resultVo.setOpenEndTime(endTime);
                                }
                            } else {
                                if (LocalTime.parse(resultVo.getOpenStartTime()).compareTo(LocalTime.parse(startTime)) > 0) {
                                    resultVo.setOpenStartTime(startTime);
                                }
                                if (LocalTime.parse(resultVo.getOpenEndTime()).compareTo(LocalTime.parse(endTime)) < 0) {
                                    resultVo.setOpenEndTime(endTime);
                                }
                            }
                        } else if (localTime.compareTo(LocalTime.parse(startTime)) < 0) {
                            if (localTime.compareTo(LocalTime.parse(resultVo.getOpenStartTime())) < 0) {
                                if (LocalTime.parse(startTime).compareTo(LocalTime.parse(resultVo.getOpenStartTime())) < 0) {
                                    resultVo.setOpenStartTime(startTime);
                                }
                            } else {
                                if (LocalTime.parse(startTime).compareTo(LocalTime.parse(resultVo.getOpenEndTime())) <= 0
                                        && LocalTime.parse(endTime).compareTo(LocalTime.parse(resultVo.getOpenEndTime())) > 0) {
                                    resultVo.setOpenEndTime(endTime);
                                }
                            }
                        }
                    }
                }
            });
        });
        if (resultVo.getOpenStartTime() == null) {
            this.setChangingTime(resultVo, ruleVOList, this.getNextDay(today));
            return;
        }
        if (localTime.compareTo(LocalTime.parse(resultVo.getOpenStartTime())) >= 0 && localTime.compareTo(LocalTime.parse(resultVo.getOpenEndTime())) <= 0) {
            resultVo.setChangingTime(resultVo.getOpenEndTime());
            if (resultVo.getOpenEndTime().equals("23:59")) {
                resultVo.setChangingTime("24:00");
            }
        }
        if (localTime.compareTo(LocalTime.parse(resultVo.getOpenStartTime())) < 0) {
            resultVo.setIsOpen(false);
            resultVo.setChangingTime(resultVo.getOpenStartTime());
        }
    }

    private void setChangingTime(RuleRelateForAppVO resultVo, List<RuleVO> ruleVOList, Integer today) {
        if (resultVo.getChangingTime() != null) {
            return;
        }
        ruleVOList.forEach(rule -> {
            List<RuleDetailVO> details = rule.getDetails();
            details.forEach(detail -> {
                List<Integer> dayList = Arrays.stream(detail.getDays().split(",")).map(Integer::valueOf).collect(Collectors.toList());
                if (dayList.contains(today)) {
                    String startTime = detail.getStartTime();
                    if (resultVo.getChangingTime() == null || LocalTime.parse(startTime).compareTo(LocalTime.parse(resultVo.getChangingTime())) < 0) {
                        resultVo.setChangingTime(startTime);
                    }
                    resultVo.setIsOpen(false);
                    resultVo.setDay(today);
                }
            });
        });
        this.setChangingTime(resultVo, ruleVOList, getNextDay(today));
    }

    private Integer getNextDay(int today) {
        if (today == 7) {
            return 1;
        }
        return today + 1;
    }

    private void setSiteLimit(RuleRelateForAppVO resultVo, List<OpLocationEvseElasticDTO> evseDtoList, List<RuleVO> ruleVOList, ZoneId zoneId) {
        Map<Long, RuleVO> ruleVoMap = ruleVOList.stream().collect(Collectors.toMap(RuleVO::getId, ruleVO -> ruleVO));
        for (OpLocationEvseElasticDTO evseDto : evseDtoList) {
            Long ruleId = evseDto.getRuleId();
            RuleVO rule = ruleVoMap.get(ruleId);
            List<RuleDetailVO> details = rule.getDetails();
            //有规则关联并且在限制时间和用户组内 场站对该用户开放
            LocalDateTime now = LocalDateTime.now(zoneId);
            int today = now.getDayOfWeek().getValue();
            for (RuleDetailVO detail : details) {
                List<Integer> dayList = Arrays.stream(detail.getDays().split(",")).map(Integer::valueOf).collect(Collectors.toList());
                //是否在限制时间范围内
                if (dayList.contains(today)) {
                    String startTime = detail.getStartTime();
                    String endTime = detail.getEndTime();
                    List<Long> memberGroupIds = Arrays.stream(detail.getMemberGroupId().split(",")).map(Long::valueOf).collect(Collectors.toList());
                    LocalTime localTime = now.toLocalTime();
                    if (localTime.compareTo(LocalTime.parse(startTime)) >= 0 && localTime.compareTo(LocalTime.parse(endTime)) <= 0) {
                        //是否在限制用户组内
                        if (!CollectionUtils.isEmpty(memberGroupIds) && this.checkGroup(memberGroupIds, resultVo.getMemberGroupIds())) {
                            resultVo.setIsLimit(false);
                            return;
                        }
                    }
                }
            }
        }
    }

    private boolean checkGroup(List<Long> memberGroupIds, List<Long> groupIds) {
        //包含all driver
        if (memberGroupIds.contains(-1L)) {
            return true;
        }
        return memberGroupIds.stream().anyMatch(groupIds::contains);
    }
}
