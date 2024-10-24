package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.date.DateTime;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.repository.LocalAuthListRepository;
import com.autel.cloud.pile.base.domain.service.LocalAuthListService;
import com.autel.cloud.pile.base.dto.AuthorizationData;
import com.autel.cloud.pile.base.dto.LocalListInformationDTO;
import com.autel.cloud.pile.base.dto.SaveAuthListDTO;
import com.autel.cloud.pile.base.dto.SendLocalListDataDTO;
import com.autel.cloud.pile.base.enums.LocalAuthListEnum;
import com.autel.cloud.pile.base.infrastructure.feign.PileUserServiceFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.ChargeCardMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocalAuthListEntity;
import com.autel.cloud.pile.base.vo.GetAllAuthListVO;
import com.autel.cloud.pile.base.vo.IdTagInfoVO;
import com.autel.cloud.pile.bill.dto.SendMsgDto;
import com.autel.cloud.pile.user.api.vo.OpenIdAndMemberIdVO;
import com.autel.cloud.smart.charge.enums.OcppAction;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@Slf4j
public class LocalAuthListServiceImpl implements LocalAuthListService {
    @Resource
    private LocalAuthListRepository localAuthListRepository;
    @Resource
    PileUserServiceFeign pileUserServiceFeign;
    @Resource
    ChargeCardMapper chargeCardMapper;

    @Override
    public List<LocalAuthListEntity> getAllAuthListBySellerId(String sn,Long sellerId) {
        if (StringUtils.isBlank(sn) || ObjectUtils.isEmpty(sellerId)) {
            return null;
        }

        return localAuthListRepository.getAllAuthListBySellerId(sn,sellerId);
    }

    @Override
    public Boolean saveAuthList(SaveAuthListDTO saveAuthListDTO) {
        return localAuthListRepository.saveAuthList(saveAuthListDTO);
    }

    @Override
    public LocalAuthListEntity getDetail(Long id) {
        return localAuthListRepository.getOne(new LambdaQueryWrapper<LocalAuthListEntity>()
         .eq(LocalAuthListEntity::getId,id));
    }

    @Override
    public Boolean updateAuthList(LocalListInformationDTO localListInformationDTO) {
        return localAuthListRepository.updateAuthList(localListInformationDTO);
    }

    @Override
    public Boolean deletedAuthList(Long id, String sn) {
        return localAuthListRepository.deletedAuthList(id,sn);
    }

    @Override
    public List<GetAllAuthListVO> getAllAuthList(String sn, Long sellerId) {
        List<LocalAuthListEntity> allAuthListBySellerId = localAuthListRepository.getAllAuthListBySellerId(sn, sellerId);
        if (CollectionUtils.isEmpty(allAuthListBySellerId)) {
            log.info("查询不到白名单列表");
            return new ArrayList<>();
        }
        List<String> cardIds = allAuthListBySellerId.stream().map(LocalAuthListEntity::getCard).collect(Collectors.toList());
        List<ChargeCardEntity> chargeCardEntityList = chargeCardMapper.selectList(new LambdaQueryWrapper<ChargeCardEntity>()
                .select(ChargeCardEntity::getId, ChargeCardEntity::getUserId,ChargeCardEntity::getCardNumber)
                .in(ChargeCardEntity::getCardNumber, cardIds)
                .eq(ChargeCardEntity::getDeleted,0)
                .eq(ChargeCardEntity::getOperatorId,sellerId));
        Map<String, Long> carNumberMap = new HashMap<>();
        List<String> userIds = new ArrayList<>();
        Map<String, Long> idMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(chargeCardEntityList)) {
            carNumberMap = chargeCardEntityList.stream().filter(m->!StringUtils.isBlank(m.getCardNumber()) && !ObjectUtils.isEmpty(m.getUserId()))
                    .collect(Collectors.toMap(ChargeCardEntity::getCardNumber, ChargeCardEntity::getUserId, (e1, e2) -> e2));
            userIds = chargeCardEntityList.stream().map(m-> String.valueOf(m.getUserId())).collect(Collectors.toList());
            Result<List<OpenIdAndMemberIdVO>> result = pileUserServiceFeign.getIdByOpenId(userIds);
            if (!ObjectUtils.isEmpty(result) && !CollectionUtils.isEmpty(result.getData())) {
                idMap = result.getData().stream().filter(m->!StringUtils.isBlank(m.getOpenId()) && !ObjectUtils.isEmpty(m.getMemberId()))
                        .collect(Collectors.toMap(OpenIdAndMemberIdVO::getOpenId, OpenIdAndMemberIdVO::getMemberId, (e1, e2) -> e2));
            }
        }

        List<GetAllAuthListVO> getAllAuthListVOS = new ArrayList<>();
        for (LocalAuthListEntity entity : allAuthListBySellerId) {
            GetAllAuthListVO vo = new GetAllAuthListVO();
            BeanUtils.copyProperties(entity,vo);
            if (!ObjectUtils.isEmpty(carNumberMap) && !ObjectUtils.isEmpty(carNumberMap.get(vo.getCard()))) {
                Long userId = carNumberMap.get(vo.getCard());
                if (!ObjectUtils.isEmpty(idMap) && !ObjectUtils.isEmpty(userId)) {
                    vo.setMemberId(idMap.get(String.valueOf(userId)));
                }
            }
            if (vo.getExpiredTime() < System.currentTimeMillis() && vo.getExpiredTime() != -1L) {
                vo.setStatus(3);
            }else if (vo.getCreateTime() < vo.getExpiredTime()) {
                vo.setStatus(1);
            }
            getAllAuthListVOS.add(vo);
        }
        return getAllAuthListVOS;
    }

    @Override
    public Boolean sendAllAuthList(SaveAuthListDTO saveAuthListDTO) {
        return localAuthListRepository.sendAllAuthList(saveAuthListDTO);
    }
}
