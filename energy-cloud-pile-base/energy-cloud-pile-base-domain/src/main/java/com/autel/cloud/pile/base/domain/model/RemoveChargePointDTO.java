package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.enums.MerchantType;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

@Data
@ApiModel
public class RemoveChargePointDTO {


    /**
     * id
     */
    @ApiModelProperty("id")
    private Long id;


    /**
     * id
     */
    @ApiModelProperty("桩SN")
    private String sn;

    @ApiModelProperty(value = "商家类型", hidden = true)
    private MerchantType merchantType;

    @ApiModelProperty("确认删除")
    private boolean confirmed;
}
