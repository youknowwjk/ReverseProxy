package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.HomeShareAddDTO;
import com.autel.cloud.pile.base.dto.HomeShareUpdateDTO;
import com.autel.cloud.pile.base.dto.OpLocationPileEvseDTO;
import com.autel.cloud.pile.base.dto.RandomDelayDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.vo.OpLimitFreeVO;
import com.autel.cloud.pile.base.vo.RandomDelayVO;
import com.autel.cloud.pile.base.vo.app.HomeShareDetailVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @author A22219
 */
public interface HomePileShareRepository extends IService<OpLocationEntity> {

    /**
     * 新建家桩共享
     *
     * @param homeShareAddDTO 添加对象
     * @return 添加对象
     */
    OpLocationEntity add(HomeShareAddDTO homeShareAddDTO);

    /**
     * 桩停止家桩共享
     *
     * @return 停止结果
     */
    Boolean pileStop(String pileSN,Long userId);

    /**
     * 用户停止家桩共享
     */
    Boolean userStop();

    /**
     * 编辑家桩共享信息
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    OpLocationEntity update(HomeShareUpdateDTO homeShareUpdateDTO);

    /**
     * 编辑地址信息
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    Boolean updateAddress(HomeShareUpdateDTO homeShareUpdateDTO);

    /**
     * 编辑个人信息
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    Boolean updatePersonalInfo(HomeShareUpdateDTO homeShareUpdateDTO);

    /**
     * 编辑价格信息
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    Boolean updateCostRule(HomeShareUpdateDTO homeShareUpdateDTO);

    /**
     * 家桩共享详情
     *
     * @return 家桩共享详情
     */
    HomeShareDetailVO detail(String pileSN);

    /**
     * 限时免费
     *
     * @return LocalDate
     */
    OpLimitFreeVO limitFree();

    /**
     * 当前用户是否有共享场站
     *
     * @return 是否有共享场站
     */
    Boolean userHaveShare();

    /**
     * 桩是否已共享
     *
     * @return 是否有共享场站
     */
    Boolean pileHaveShare(String pileSN);

    /**
     * 用户所有已共享的桩
     *
     * @return 用户所有已共享的桩
     */
    List<String> userHaveSharePileList();

    /**
     * 更新限时免费
     *
     * @return
     */
    OpLimitFreeVO updateLimitFree(Long freeDate);

    /**
     * 新增限时免费
     *
     * @return
     */
    OpLimitFreeVO addLimitFree(Long freeDate);

    /**
     * 随机延迟开关
     */
    Boolean randomDelay(RandomDelayDTO randomDelayDTO);

    /**
     * 是否开启随机延迟
     */
    RandomDelayVO randomDelayQuery(String pileSN);

    Boolean userStopV2(Long userId);

    Boolean updateHomePileInfo(OpLocationPileEvseDTO opLocationPileEvseDTO);
}
