package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.date.DateTime;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.repository.LocalAuthListRepository;
import com.autel.cloud.pile.base.dto.AuthorizationData;
import com.autel.cloud.pile.base.dto.LocalListInformationDTO;
import com.autel.cloud.pile.base.dto.SaveAuthListDTO;
import com.autel.cloud.pile.base.dto.SendLocalListDataDTO;
import com.autel.cloud.pile.base.enums.LocalAuthListEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.feign.WxProxyClient;
import com.autel.cloud.pile.base.infrastructure.mapper.LocalAuthListMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocalAuthListEntity;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.vo.IdTagInfoVO;
import com.autel.cloud.pile.bill.dto.SendMsgDto;
import com.autel.cloud.smart.charge.enums.OcppAction;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

@Service
@Log4j2
public class LocalAuthListRepositoryImpl extends ServiceImpl<LocalAuthListMapper, LocalAuthListEntity> implements LocalAuthListRepository {
    @Resource
    LocalAuthListMapper localAuthListMapper;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private WxProxyClient wxProxyClient;
    @Override
    public List<LocalAuthListEntity> getAllAuthListBySellerId(String sn,Long sellerId) {
        return localAuthListMapper.selectList(new LambdaQueryWrapper<LocalAuthListEntity>()
           .eq(LocalAuthListEntity::getSn,sn)
           .eq(LocalAuthListEntity::getSellerId,sellerId));
    }

    @Override
    public Boolean saveAuthList(SaveAuthListDTO saveAuthListDTO) {
        log.info("saveAuthList,saveAuthListDTO={}",JSON.toJSON(saveAuthListDTO));
        localAuthListMapper.delete(new LambdaQueryWrapper<LocalAuthListEntity>()
                .eq(LocalAuthListEntity::getSn,saveAuthListDTO.getSn())
                .eq(LocalAuthListEntity::getSellerId,saveAuthListDTO.getSellerId()));
        String s = stringRedisTemplate.opsForValue().get(RedisKeyConstant.sendLocalListDataKey(saveAuthListDTO.getSn() + "_" + saveAuthListDTO.getSeq()));
        log.info("saveAuthList,s={}",s);
        if (StringUtils.isBlank(s) || "[]".equals(s)) {
            log.info("saveAuthList,redis缓存过期");
            return null;
        }
        List<LocalAuthListEntity> localAuthListEntities = JSON.parseArray(s,LocalAuthListEntity.class);
        for (LocalAuthListEntity localAuthListEntity : localAuthListEntities) {
            if (ObjectUtils.isEmpty(localAuthListEntity.getCreateTime())) {
                localAuthListEntity.setCreateTime(System.currentTimeMillis());
            }
            localAuthListEntity.setUpdateTime(System.currentTimeMillis());
            localAuthListEntity.setId(IdWorker.getId());
        }
        return localAuthListMapper.batchSave(localAuthListEntities);
    }

    @Override
    public Boolean updateAuthList(LocalListInformationDTO localListInformationDTO) {
        log.info("updateAuthList,localListInformationDTO={}",JSON.toJSON(localListInformationDTO));
        if (ObjectUtils.isEmpty(localListInformationDTO) || StringUtils.isBlank(localListInformationDTO.getSn())) {
            return false;
        }
        if (StringUtils.isBlank(localListInformationDTO.getCard())) {
            return true;
        }
        if (!CommonUtil.checkChargeCard(localListInformationDTO.getCard())) {
            throw new MessageCodeException(PileBaseEnum.CARD_NUMBER_ERROR);
        }
        //处理前端传入的新数据
        List<AuthorizationData> localAuthorizationList = new ArrayList<>();
        AuthorizationData authorizationData = AuthorizationData.builder()
                .idTag(localListInformationDTO.getCard())
                .idTagInfo(IdTagInfoVO.builder()
                        .status(LocalAuthListEnum.findValue(localListInformationDTO.getStatus())).build()).build();
        if (ObjectUtils.isNotEmpty(localListInformationDTO.getValidityPeriod()) && !localListInformationDTO.getValidityPeriod().equals(-1L)) {
            authorizationData.getIdTagInfo().setExpiryDate(DateTime.of(localListInformationDTO.getValidityPeriod()));
        }
        localAuthorizationList.add(authorizationData);

        //查询商家底下所有的白名单列表
        List<LocalAuthListEntity> allAuthListBySellerId = localAuthListMapper.selectList(new LambdaQueryWrapper<LocalAuthListEntity>()
                .eq(LocalAuthListEntity::getSn,localListInformationDTO.getSn())
                .eq(LocalAuthListEntity::getSellerId,LoginUserHolder.getLoginUser().getPayload().getSellerId())
                .ne(LocalAuthListEntity::getId,localListInformationDTO.getId()));

        if (!CollectionUtils.isEmpty(allAuthListBySellerId)) {
            for (LocalAuthListEntity localAuthListEntity : allAuthListBySellerId) {
                AuthorizationData data = AuthorizationData.builder()
                        .idTag(localAuthListEntity.getCard())
                        .idTagInfo(IdTagInfoVO.builder()
                                .status(LocalAuthListEnum.findValue(localAuthListEntity.getStatus())).build()).build();
                if (ObjectUtils.isNotEmpty(localAuthListEntity.getExpiredTime()) && !localAuthListEntity.getExpiredTime().equals(-1L)) {
                    data.getIdTagInfo().setExpiryDate(DateTime.of(localAuthListEntity.getExpiredTime()));
                }
                localAuthorizationList.add(data);
            }
        }
        SendLocalListDataDTO sendLocalListDataDTO = SendLocalListDataDTO.builder()
                .listVersion(4)
                .updateType("Full")
                .localAuthorizationList(localAuthorizationList).build();
        SendMsgDto sendMsgDto = new SendMsgDto();
        sendMsgDto.setReceiver("sn-"+localListInformationDTO.getSn());
        String seq = IdWorker.getIdStr();
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListKey(localListInformationDTO.getSn() + "_" +seq), LoginUserHolder.getLoginUser().getPayload().getSellerId().toString(), 2, TimeUnit.MINUTES);
        //保存全部白名单列表
        LocalAuthListEntity listEntity = localAuthListMapper.selectOne(new LambdaQueryWrapper<LocalAuthListEntity>()
                .eq(LocalAuthListEntity::getId,localListInformationDTO.getId()));
        LocalAuthListEntity entity = new LocalAuthListEntity();
        entity.setCard(localListInformationDTO.getCard());
        entity.setExpiredTime(localListInformationDTO.getValidityPeriod());
        entity.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        entity.setStatus(localListInformationDTO.getStatus());
        entity.setSn(localListInformationDTO.getSn());
        entity.setCreateTime(listEntity.getCreateTime());
        allAuthListBySellerId.add(entity);
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListUserIdKey(localListInformationDTO.getSn() + "_" +seq), LoginUserHolder.getLoginUser().getId().toString(), 2, TimeUnit.MINUTES);
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListDataKey(localListInformationDTO.getSn() + "_" +seq), JSON.toJSONString(allAuthListBySellerId), 2, TimeUnit.MINUTES);
        sendMsgDto.setMsg(buildConfigurationMsg(OcppAction.SEND_LOCAL_LIST.getValue(), JSON.toJSONString(sendLocalListDataDTO), seq));
        log.info("updateAuthList,sendMsgDto={}",JSON.toJSONString(sendMsgDto));
        return wxProxyClient.sendMsg(sendMsgDto).getData();
    }

    @Override
    public Boolean deletedAuthList(Long id, String sn) {
        if (ObjectUtils.isEmpty(id)) {
            return false;
        }
        //处理前端传入的新数据
        List<AuthorizationData> localAuthorizationList = new ArrayList<>();
        //查询商家底下所有的白名单列表
        List<LocalAuthListEntity> allAuthListBySellerId = localAuthListMapper.selectList(new LambdaQueryWrapper<LocalAuthListEntity>()
                .eq(LocalAuthListEntity::getSellerId,LoginUserHolder.getLoginUser().getPayload().getSellerId())
                .eq(LocalAuthListEntity::getSn,sn)
                .ne(LocalAuthListEntity::getId,id));

        if (!CollectionUtils.isEmpty(allAuthListBySellerId)) {
            for (LocalAuthListEntity localAuthListEntity : allAuthListBySellerId) {
                AuthorizationData data = AuthorizationData.builder()
                        .idTag(localAuthListEntity.getCard())
                        .idTagInfo(IdTagInfoVO.builder()
                                .status(LocalAuthListEnum.findValue(localAuthListEntity.getStatus())).build()).build();
                if (ObjectUtils.isNotEmpty(localAuthListEntity.getExpiredTime()) && !localAuthListEntity.getExpiredTime().equals(-1L)) {
                    data.getIdTagInfo().setExpiryDate(DateTime.of(localAuthListEntity.getExpiredTime()));
                }
                localAuthorizationList.add(data);
            }
        }
        SendLocalListDataDTO sendLocalListDataDTO = SendLocalListDataDTO.builder()
                .listVersion(4)
                .updateType("Full")
                .localAuthorizationList(localAuthorizationList).build();
        SendMsgDto sendMsgDto = new SendMsgDto();
        sendMsgDto.setReceiver("sn-"+sn);
        String seq = IdWorker.getIdStr();
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListUserIdKey(sn + "_" +seq), LoginUserHolder.getLoginUser().getId().toString(), 2, TimeUnit.MINUTES);
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListKey(sn + "_" +seq), LoginUserHolder.getLoginUser().getPayload().getSellerId().toString(), 2, TimeUnit.MINUTES);
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListDataKey(sn + "_" +seq), JSON.toJSONString(allAuthListBySellerId), 2, TimeUnit.MINUTES);
        sendMsgDto.setMsg(buildConfigurationMsg(OcppAction.SEND_LOCAL_LIST.getValue(), JSON.toJSONString(sendLocalListDataDTO), seq));
        log.info("deletedAuthList,sendMsgDto={}",JSON.toJSONString(sendMsgDto));
        return wxProxyClient.sendMsg(sendMsgDto).getData();
    }

    @Override
    public Boolean sendAllAuthList(SaveAuthListDTO saveAuthListDTO) {
        List<AuthorizationData> localAuthorizationList = new ArrayList<>();
        //查询商家底下所有的白名单列表
        List<LocalAuthListEntity> allAuthListBySellerId = this.getAllAuthListBySellerId(saveAuthListDTO.getSn(), Long.valueOf(saveAuthListDTO.getSellerId()));
        if (!org.springframework.util.CollectionUtils.isEmpty(allAuthListBySellerId)) {
            for (LocalAuthListEntity localAuthListEntity : allAuthListBySellerId) {
                AuthorizationData data = AuthorizationData.builder()
                        .idTag(localAuthListEntity.getCard())
                        .idTagInfo(IdTagInfoVO.builder()
                                .status(LocalAuthListEnum.findValue(localAuthListEntity.getStatus())).build()).build();
                if (ObjectUtils.isNotEmpty(localAuthListEntity.getExpiredTime()) && !localAuthListEntity.getExpiredTime().equals(-1L)) {
                    data.getIdTagInfo().setExpiryDate(DateTime.of(localAuthListEntity.getExpiredTime()));
                }
                localAuthorizationList.add(data);
            }
            SendLocalListDataDTO sendLocalListDataDTO = SendLocalListDataDTO.builder()
                    .listVersion(4)
                    .updateType("Full")
                    .localAuthorizationList(localAuthorizationList).build();
            SendMsgDto sendMsgDto1 = new SendMsgDto();
            sendMsgDto1.setReceiver("sn-"+saveAuthListDTO.getSn());
            String seq1 = IdWorker.getIdStr();
            stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListKey(saveAuthListDTO.getSn() + "_" +saveAuthListDTO.getSeq()), saveAuthListDTO.getSellerId(), 2, TimeUnit.MINUTES);
            stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListDataKey(saveAuthListDTO.getSn() + "_" +saveAuthListDTO.getSeq()), JSON.toJSONString(allAuthListBySellerId), 2, TimeUnit.MINUTES);
            sendMsgDto1.setMsg(buildConfigurationMsg(OcppAction.SEND_LOCAL_LIST.getValue(), JSON.toJSONString(sendLocalListDataDTO), saveAuthListDTO.getSeq()));
            log.info("sendLocalList,sendMsgDto={}",JSON.toJSONString(sendMsgDto1));
            wxProxyClient.sendMsg(sendMsgDto1);
        }
        return true;
    }

    private String buildConfigurationMsg(String action, String data, String seq) {
        return String.format("[2,\"%s\",\"%s\",%s]", seq, action,data);
    }
}
