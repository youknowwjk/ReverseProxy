package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author 吴超
 * @date 2021/6/25 13:42
 */
@Data
@ApiModel(value = "TransactionDto", description = "充电启停请求对象")
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TransactionDto {

    @ApiModelProperty(value = "充电记录ID")
    private String transactionId;

    @ApiModelProperty(value = "充电桩SN号，长度32位")
    private String chargePileSn;

    @ApiModelProperty(value = "枪编号，长度11位", example = "0")
    private Integer gunNo;

    @ApiModelProperty(value = "用户ID", required = false)
    private String userId;

}
