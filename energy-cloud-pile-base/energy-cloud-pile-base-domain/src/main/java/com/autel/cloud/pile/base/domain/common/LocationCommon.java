package com.autel.cloud.pile.base.domain.common;

import com.autel.cloud.base.http.pojo.Result;

import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Map;

/**
 * @Author MingLong A22599
 * @Date 2022/11/25
 * @Function 处理场站信息的公共服务 抽象接口
 */
public interface LocationCommon {

    /**
     * @param latitude   纬度值 不能为空
     * @param longitude  经度值 不能为空
     * @param locationId 场站id 新增时无场站id, 修改时才有场站id
     * @return 设置的结果
     * @function 设置纬度和经度
     */
    Map<String, String> setTheLatitudeAndLongitudeOfTheStation(@NotNull String latitude, @NotNull String longitude, Long locationId);


    /**
     * 版本号比较
     */
    int compareVersion(String userVersion, String supportUpgradeVersion);

    /**
     * 下发计费规则
     *
     * @param tariffId 计费规则id
     * @param userId 用户id
     * @param newRelease 新版下发标志
     * @return
     */
    Result<Boolean> dispatchTariffOfPublicPileByTariffId(Long tariffId, Long userId, Boolean newRelease);

    Boolean issueBillingRule(List<String> pileSnList);
}
