package com.autel.cloud.pile.base.domain.model.dto.gun;

import com.autel.cloud.base.common.page.PageDTO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@ApiModel("为计费规则组查询枪信息 入参模型")
public class SelectGunInfoForTariffGroupIdDTO extends PageDTO implements Serializable {

    @ApiModelProperty(value = "商户id", hidden = true)
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long sellerId;

    @ApiModelProperty(value = "计费规则组id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long tariffGroupId;

    @ApiModelProperty(value = "场站id集合")
    private List<Long> locationIdList;
}
