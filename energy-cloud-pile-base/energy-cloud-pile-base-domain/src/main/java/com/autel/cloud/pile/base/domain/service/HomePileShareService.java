package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.HomeShareAddDTO;
import com.autel.cloud.pile.base.dto.HomeShareUpdateDTO;
import com.autel.cloud.pile.base.dto.OpLocationPileEvseDTO;
import com.autel.cloud.pile.base.dto.RandomDelayDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.vo.OpLimitFreeVO;
import com.autel.cloud.pile.base.vo.RandomDelayVO;
import com.autel.cloud.pile.base.vo.app.HomeShareDetailVO;

import java.util.List;

/**
 * @author A22219
 */
public interface HomePileShareService {

    /**
     * 新建家桩共享
     *
     * @param homeShareAddDTO 添加对象
     * @return 添加结果
     */
    Result<OpLocationEntity> add(HomeShareAddDTO homeShareAddDTO);

    /**
     * 停止家桩共享
     *
     * @return 停止结果
     */
    Result<Boolean> pileStop(String pileSN);

    /**
     * 编辑家桩共享信息
     */
    Result<OpLocationEntity> update(HomeShareUpdateDTO homeShareUpdateDTO);

    /**
     * 编辑地址信息
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    Result<Boolean> updateAddress(HomeShareUpdateDTO homeShareUpdateDTO);

    /**
     * 编辑个人信息
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    Result<Boolean> updatePersonalInfo(HomeShareUpdateDTO homeShareUpdateDTO);

    /**
     * 编辑价格信息
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    Result<Boolean> updateCostRule(HomeShareUpdateDTO homeShareUpdateDTO);

    /**
     * 家桩共享详情
     *
     * @return 家桩共享详情
     */
    Result<HomeShareDetailVO> detail(String pileSN);

    /**
     * 限时免费
     *
     * @return 限时免费
     */
    Result<OpLimitFreeVO> limitFree();

    /**
     * 当前用户是否有共享场站
     *
     * @return 是否有共享场站
     */
    Result<Boolean> userHaveShare();

    /**
     * 桩是否已共享
     *
     * @return 是否有共享场站
     */
    Result<Boolean> pileHaveShare(String pileSN);

    /**
     * 用户所有已共享的桩
     *
     * @return 用户所有已共享的桩
     */
    Result<List<String>> userHaveSharePileList();

    /**
     * 更新限时免费
     *
     * @return 限时免费
     */
    Result<OpLimitFreeVO> updateLimitFree(Long freeDate);

    /**
     * 新增限时免费
     *
     * @return
     */
    Result<OpLimitFreeVO> addLimitFree(Long freeDate);

    /**
     * 随机延迟开关
     */
    Result<Boolean> randomDelay(RandomDelayDTO randomDelayDTO);

    /**
     * 是否开启随机延迟
     */
    Result<RandomDelayVO> randomDelayQuery(String pileSN);

    /**
     * 用户停止家桩共享
     */
    Result<Boolean> userStop();

    Result<Boolean> userStopV2(Long userId);

    Result<Boolean> updateHomePileInfo(OpLocationPileEvseDTO opLocationPileEvseDTO);
}
