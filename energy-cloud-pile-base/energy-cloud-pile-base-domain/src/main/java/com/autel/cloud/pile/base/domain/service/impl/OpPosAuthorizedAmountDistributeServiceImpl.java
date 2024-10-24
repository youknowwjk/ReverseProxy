package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.exception.BusinessException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.config.ops.request.DownConfigMsgToOpsDTO;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.repository.OpPosAuthorizedAmountDistributeRepositoty;
import com.autel.cloud.pile.base.domain.service.OpPosAuthorizedAmountDistributeService;
import com.autel.cloud.pile.base.domain.utils.CheckUtils;
import com.autel.cloud.pile.base.dto.pos.SetPosAuthorizedAmountDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.enums.ops.ConfigMsgActionEnum;
import com.autel.cloud.pile.base.enums.ops.ConfigMsgReqSrcEnum;
import com.autel.cloud.pile.base.infrastructure.feign.OpsApiClient;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPosAuthorizedAmountDistributeEntity;
import com.autel.cloud.pile.base.vo.pos.PileVO;
import com.autel.cloud.pile.base.vo.pos.PosAuthorizedAmountDistributeVO;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * @Author A22599
 * @Date 2023/06/02
 * @Function 本地POS预授权金额修改工具设置 业务逻辑层 实现类
 */
@Service
@Slf4j
public class OpPosAuthorizedAmountDistributeServiceImpl implements OpPosAuthorizedAmountDistributeService {

    @Autowired
    private OpPosAuthorizedAmountDistributeRepositoty opPosAuthorizedAmountDistributeRepositoty;

    @Autowired
    private MonitorFeignClient monitorFeignClient;

    @Autowired
    private OpsApiClient opsApiClient;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;


    public static final BigDecimal _100 = new BigDecimal("100");

    /**
     * 本地POS机预授权金额区间上限
     */
    public static final BigDecimal PRE_AUTHORIZATION_AMOUNT_UPPER_LIMIT = new BigDecimal("65535");

    /**
     * 本地POS机预授权金额区间下限
     */
    public static final BigDecimal PRE_AUTHORIZATION_AMOUNT_LOWER_LIMIT = new BigDecimal("1000");

    /**
     * @param pageDTO
     * @return
     * @function 分页查询本地POS预授权金额修改工具设置记录
     */
    @Override
    public Page<PosAuthorizedAmountDistributeVO> queryPages(PageDTO pageDTO) {

        log.info("=====>>>>>OpPosAuthorizedAmountDistributeServiceImpl.queryPages pageDTO : {}",
                JSON.toJSONString(pageDTO));

        if (pageDTO == null
                || pageDTO.getPage() == null
                || pageDTO.getPageSize() == null) {
            return new Page<>();
        }

        Page<PosAuthorizedAmountDistributeVO> resultPage = new Page<>(pageDTO.getPage(), pageDTO.getPageSize());
        Page<OpPosAuthorizedAmountDistributeEntity> opPosAuthorizedAmountDistributeEntityPage = opPosAuthorizedAmountDistributeRepositoty.queryPages(pageDTO);
        if (opPosAuthorizedAmountDistributeEntityPage != null
                && ObjectUtils.isNotEmpty(opPosAuthorizedAmountDistributeEntityPage.getRecords())) {
            List<PosAuthorizedAmountDistributeVO> posAuthorizedAmountDistributeVOList = new ArrayList<>();
            opPosAuthorizedAmountDistributeEntityPage.getRecords().forEach(var -> {
                PosAuthorizedAmountDistributeVO posAuthorizedAmountDistributeVO = new PosAuthorizedAmountDistributeVO();
                BeanUtils.copyProperties(var, posAuthorizedAmountDistributeVO);
                posAuthorizedAmountDistributeVOList.add(posAuthorizedAmountDistributeVO);
            });
            resultPage.setTotal(opPosAuthorizedAmountDistributeEntityPage.getTotal());
            resultPage.setRecords(posAuthorizedAmountDistributeVOList);
        }
        return resultPage;
    }

    /**
     * @param setPosAuthorizedAmountDTO
     * @return
     * @function 修改本地POS预授权金额
     */
    @Override
    public Boolean setPosAuthorizedAmount(SetPosAuthorizedAmountDTO setPosAuthorizedAmountDTO) {

        log.info("=====>>>>>OpPosAuthorizedAmountDistributeServiceImpl.setPosAuthorizedAmount setPosAuthorizedAmountDTO : {}",
                JSON.toJSONString(setPosAuthorizedAmountDTO));

        Long id = IdWorker.getId();
        String pileSn = setPosAuthorizedAmountDTO.getPileSn();
        BigDecimal authorizedAmount = setPosAuthorizedAmountDTO.getAuthorizedAmount();
        String msgId = setPosAuthorizedAmountDTO.getMsgId();
        Long currentTimeMillis = System.currentTimeMillis();

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        Long sellerId = null;
        Long userId = null;
        if (jwtInfo != null
                && jwtInfo.getPayload() != null) {
            sellerId = jwtInfo.getPayload().getSellerId();
            userId = jwtInfo.getPayload().getUserId();
        }

        BigDecimal preCurrency = authorizedAmount.multiply(BigDecimal.valueOf(100)).stripTrailingZeros();
        this.checkAuthorizedAmount(preCurrency);

        OpPosAuthorizedAmountDistributeEntity opPosAuthorizedAmountDistributeEntity = new OpPosAuthorizedAmountDistributeEntity();
        opPosAuthorizedAmountDistributeEntity.setId(id);
        opPosAuthorizedAmountDistributeEntity.setPileSn(pileSn);
        opPosAuthorizedAmountDistributeEntity.setStatus(0);
        opPosAuthorizedAmountDistributeEntity.setSellerId(sellerId);
        opPosAuthorizedAmountDistributeEntity.setUpdateBy(userId);
        opPosAuthorizedAmountDistributeEntity.setCreateBy(userId);
        opPosAuthorizedAmountDistributeEntity.setDeleted(0);
        opPosAuthorizedAmountDistributeEntity.setCreateTime(currentTimeMillis);
        opPosAuthorizedAmountDistributeEntity.setUpdateTime(currentTimeMillis);
        this.saveOrUpdatePosAuthorizedAmountDistribute(opPosAuthorizedAmountDistributeEntity);

        com.autel.cloud.pile.base.config.ops.content.SetPosAuthorizedAmountDTO setPosAuthorizedAmountDTOContent = new com.autel.cloud.pile.base.config.ops.content.SetPosAuthorizedAmountDTO();
        setPosAuthorizedAmountDTOContent.setPreCurrency(preCurrency.toPlainString());
        setPosAuthorizedAmountDTOContent.setId(id);
        DownConfigMsgToOpsDTO downConfigMsgToOpsDTO = new DownConfigMsgToOpsDTO();
        downConfigMsgToOpsDTO.setMsgId(msgId);
        downConfigMsgToOpsDTO.setAction(ConfigMsgActionEnum.POS_PRE_SALE_AMOUNT.getActionName());
        downConfigMsgToOpsDTO.setParam(setPosAuthorizedAmountDTOContent);
        downConfigMsgToOpsDTO.setSn(pileSn);
        downConfigMsgToOpsDTO.setAppUser(userId == null ? null : String.valueOf(userId.longValue()));
        downConfigMsgToOpsDTO.setReqSrc(ConfigMsgReqSrcEnum.APP.getReqSrcName());

        log.info("===>>>OpPosAuthorizedAmountDistributeServiceImpl.setPosAuthorizedAmount downConfigMsgToOpsDTO : {}",
                JSON.toJSONString(downConfigMsgToOpsDTO));

        Result<Boolean> downConfigMessageResult = opsApiClient.downConfigMessage(downConfigMsgToOpsDTO);

        log.info("===>>>OpPosAuthorizedAmountDistributeServiceImpl.setPosAuthorizedAmount downConfigMessageResult : {}",
                JSON.toJSONString(downConfigMessageResult));

        stringRedisTemplate.opsForValue().set(RedisKeyConstant.getSetPosAuthorizedAmountKey(msgId), JSON.toJSONString(downConfigMsgToOpsDTO), 2L, TimeUnit.MINUTES);
        return downConfigMessageResult.getData();
    }

    private void checkAuthorizedAmount(BigDecimal authorizedAmount) {

        log.info("===>>> OpPosAuthorizedAmountDistributeServiceImpl.checkAuthorizedAmount authorizedAmount : {}",
                JSON.toJSONString(authorizedAmount));

        if (authorizedAmount != null
                && !CheckUtils.checkLowerLimit(OpPosAuthorizedAmountDistributeServiceImpl.PRE_AUTHORIZATION_AMOUNT_LOWER_LIMIT, authorizedAmount, true)) {
            throw new MessageCodeException(PileBaseEnum.AMOUNT_TOO_SMALL);
        }
        if (authorizedAmount != null
                && !CheckUtils.checkUpperLimit(OpPosAuthorizedAmountDistributeServiceImpl.PRE_AUTHORIZATION_AMOUNT_UPPER_LIMIT, authorizedAmount, true)) {
            throw new MessageCodeException(PileBaseEnum.AMOUNT_TOO_BIG);
        }
    }

    /**
     * @param opPosAuthorizedAmountDistributeEntity
     * @return
     * @function 新增或者修改一条本地POS预授权金额修改工具设置记录
     */
    @Override
    public Boolean saveOrUpdatePosAuthorizedAmountDistribute(OpPosAuthorizedAmountDistributeEntity opPosAuthorizedAmountDistributeEntity) {

        log.info("=====>>>>>OpPosAuthorizedAmountDistributeServiceImpl.saveOrUpdatePosAuthorizedAmountDistribute opPosAuthorizedAmountDistributeEntity : {}",
                JSON.toJSONString(opPosAuthorizedAmountDistributeEntity));

        return opPosAuthorizedAmountDistributeRepositoty.saveOrUpdatePosAuthorizedAmountDistribute(opPosAuthorizedAmountDistributeEntity);
    }

    /**
     * @param pageDTO
     * @return
     * @function 分页查询充电桩序列号下拉列表
     */
    @Override
    public Page<PileVO> getPileDropDownPageList(PageDTO pageDTO) {

        log.info("=====>>>>>OpPosAuthorizedAmountDistributeServiceImpl.getPileDropDownPageList pageDTO : {}",
                JSON.toJSONString(pageDTO));

        Result<Page<PileVO>> result = opsApiClient.getPileDropDownPageList(pageDTO);

        log.info("=====>>>>>OpPosAuthorizedAmountDistributeServiceImpl.getPileDropDownPageList result : {}",
                JSON.toJSONString(result));

        return result.getData();
    }
}
