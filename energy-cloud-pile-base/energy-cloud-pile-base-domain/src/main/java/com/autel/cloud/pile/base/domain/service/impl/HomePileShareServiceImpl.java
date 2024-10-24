package com.autel.cloud.pile.base.domain.service.impl;


import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.HomePileShareRepository;
import com.autel.cloud.pile.base.domain.service.HomePileShareService;
import com.autel.cloud.pile.base.dto.HomeShareAddDTO;
import com.autel.cloud.pile.base.dto.HomeShareUpdateDTO;
import com.autel.cloud.pile.base.dto.OpLocationPileEvseDTO;
import com.autel.cloud.pile.base.dto.RandomDelayDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.vo.OpLimitFreeVO;
import com.autel.cloud.pile.base.vo.RandomDelayVO;
import com.autel.cloud.pile.base.vo.app.HomeShareDetailVO;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author A22219
 */
@Service
@Log4j2
public class HomePileShareServiceImpl implements HomePileShareService {

    @Resource
    private HomePileShareRepository homePileShareRepository;

    /**
     * 新建家桩共享家桩共享
     *
     * @param homeShareAddDTO 添加对象
     * @return 新建结果
     */
    @Override
    public Result<OpLocationEntity> add(HomeShareAddDTO homeShareAddDTO) {
        OpLocationEntity opLocationEntity = homePileShareRepository.add(homeShareAddDTO);
        if (opLocationEntity == null) {
            // 设置失败
            throw new MessageCodeException(PileBaseEnum.ADD_MEMBER_CUSTOMER_FAIL);
        }
        return Result.ofSucceed(opLocationEntity);
    }

    /**
     * 桩停止家桩共享
     *
     * @return 停止结果
     */
    @Override
    public Result<Boolean> pileStop(String pileSN) {
        return Result.ofSucceed(homePileShareRepository.pileStop(pileSN,null));
    }

    /**
     * 编辑家桩共享信息
     */
    @Override
    public Result<OpLocationEntity> update(HomeShareUpdateDTO homeShareUpdateDTO) {
        return Result.ofSucceed(homePileShareRepository.update(homeShareUpdateDTO));
    }

    /**
     * 编辑家桩共享信息
     */
    @Override
    public Result<Boolean> updateAddress(HomeShareUpdateDTO homeShareUpdateDTO) {
        return Result.ofSucceed(homePileShareRepository.updateAddress(homeShareUpdateDTO));
    }

    /**
     * 编辑家桩共享信息
     */
    @Override
    public Result<Boolean> updatePersonalInfo(HomeShareUpdateDTO homeShareUpdateDTO) {
        return Result.ofSucceed(homePileShareRepository.updatePersonalInfo(homeShareUpdateDTO));
    }

    /**
     * 编辑家桩共享信息
     */
    @Override
    public Result<Boolean> updateCostRule(HomeShareUpdateDTO homeShareUpdateDTO) {
        return Result.ofSucceed(homePileShareRepository.updateCostRule(homeShareUpdateDTO));
    }

    /**
     * 家桩共享详情
     *
     * @return 家桩共享详情
     */
    @Override
    public Result<HomeShareDetailVO> detail(String pileSN) {
        return Result.ofSucceed(homePileShareRepository.detail(pileSN));
    }

    /**
     * 限时免费
     *
     * @return
     */
    @Override
    public Result<OpLimitFreeVO> limitFree() {
        return Result.ofSucceed(homePileShareRepository.limitFree());
    }

    /**
     * 当前用户是否有共享场站
     *
     * @return 是否有共享场站
     */
    @Override
    public Result<Boolean> userHaveShare() {
        return Result.ofSucceed(homePileShareRepository.userHaveShare());
    }

    /**
     * 桩是否已共享
     *
     * @return 是否有共享场站
     */
    @Override
    public Result<Boolean> pileHaveShare(String pileSN) {
        return Result.ofSucceed(homePileShareRepository.pileHaveShare(pileSN));
    }

    /**
     * 用户所有已共享的桩
     *
     * @return 用户所有已共享的桩
     */
    @Override
    public Result<List<String>> userHaveSharePileList() {
        return Result.ofSucceed(homePileShareRepository.userHaveSharePileList());
    }

    /**
     * 更新限时免费
     *
     * @return
     */
    @Override
    public Result<OpLimitFreeVO> updateLimitFree(Long freeDate) {
        return Result.ofSucceed(homePileShareRepository.updateLimitFree(freeDate));
    }

    /**
     * 新增限时免费
     *
     * @return
     */
    @Override
    public Result<OpLimitFreeVO> addLimitFree(Long freeDate) {
        return Result.ofSucceed(homePileShareRepository.addLimitFree(freeDate));
    }

    /**
     * 随机延迟开关
     */
    @Override
    public Result<Boolean> randomDelay(RandomDelayDTO randomDelayDTO) {
        return Result.ofSucceed(homePileShareRepository.randomDelay(randomDelayDTO));
    }

    /**
     * 是否开启随机延迟
     */
    @Override
    public Result<RandomDelayVO> randomDelayQuery(String pileSN) {
        return Result.ofSucceed(homePileShareRepository.randomDelayQuery(pileSN));
    }

    /**
     * 用户停止家桩共享
     */
    @Override
    public Result<Boolean> userStop() {
        return Result.ofSucceed(homePileShareRepository.userStop());
    }

    @Override
    public Result<Boolean> userStopV2(Long userId) {
        return Result.ofSucceed(homePileShareRepository.userStopV2(userId));
    }

    @Override
    public Result<Boolean> updateHomePileInfo(OpLocationPileEvseDTO opLocationPileEvseDTO) {
        return Result.ofSucceed(homePileShareRepository.updateHomePileInfo(opLocationPileEvseDTO));
    }
}
