package com.autel.cloud.pile.base.domain.repository;

import java.util.List;

/**
 * @ClassName OpLocationEvseOperationRepository
 * @Author A22121
 * @Description
 * @Date 2022/4/27 20:03
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpLocationEvseOperationRepository {
    /**
     * 版本更新
     * @param evseSn
     * @return
     */
    Boolean updateVersion(String evseSn);

    /**
     * 批量版本更新
     * @param evseSnList
     * @return
     */
    Boolean batchUpdateVersion(List<String> evseSnList);

    /**
     * 禁用充电
     * @param evseSn
     * @return
     */
    Boolean disable(String evseSn);

    /**
     * 远程停止充电
     * @param evseSn
     * @param busId
     * @param userId
     * @return
     */
    Boolean backendStop(String evseSn, String busId, Long userId);

    /**
     * 解禁充电
     * @param evseSn
     * @return
     */
    Boolean able(String evseSn);

    /**
     * 枪重启
     * @param evseSn
     * @return
     */
    Boolean reset(String evseSn);

    /**
     * 枪解锁
     * @param evseSn
     * @return
     */
    Boolean unlock(String evseSn);
}
