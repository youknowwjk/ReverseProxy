package com.autel.cloud.pile.base.infrastructure.feign.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Author:   A19011
 * Description: LockOrUnlockGunSupportInfoVO
 * Date:     2024/2/22 17:58
 *
 * @Version 0.0.1-SNAPSHOT
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class LockOrUnlockGunSupportInfoVO {
    @ApiModelProperty("充电桩sn")
    private String sn;
    @ApiModelProperty("是否支持解锁枪")
    private boolean lockOrUnlockGunSupportEnabled;
    @ApiModelProperty("是否需要升级")
    private boolean requireToUpgradEnabled;
}
