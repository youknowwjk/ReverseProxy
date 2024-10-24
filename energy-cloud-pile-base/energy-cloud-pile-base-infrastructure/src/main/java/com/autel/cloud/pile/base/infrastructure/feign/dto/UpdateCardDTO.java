package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

/**
 * @Author: X22010
 * @Date: 2022/08/17/15:11
 * @Description:
 */
@Data
@ApiModel("更新卡")
public class UpdateCardDTO {
    @ApiModelProperty(value = "卡名，最大长度：100",notes = "卡名")
    @Size(max = 100)
    private String name;

    @ApiModelProperty(value = "卡号，最大长度：100",notes = "卡号")
    @NotBlank
    @Size(max = 100)
    private String cardNumber;
}
