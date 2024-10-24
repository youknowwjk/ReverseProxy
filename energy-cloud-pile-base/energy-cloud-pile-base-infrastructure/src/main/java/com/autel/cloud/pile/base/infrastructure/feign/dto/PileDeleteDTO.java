package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * <p>
 * 充电桩DTO
 * </p>
 *
 * @author William
 * @since 2022/6/24
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PileDeleteDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * SN编号
     */
    @ApiModelProperty("SN编号")
    private String sn;

    /**
     * 商家id
     */
    @ApiModelProperty("商家id")
    private String sellerId;

    /**
     * 场站所属组织id
     */
    @ApiModelProperty("场站所属组织id")
    private Long groupId;

    /**
     * 运营二次删除确认标识
     */
    @ApiModelProperty("运营二次删除确认标识")
    private Boolean deleteFlag;
}
