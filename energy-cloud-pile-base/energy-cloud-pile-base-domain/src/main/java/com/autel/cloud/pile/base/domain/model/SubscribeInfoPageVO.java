package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.base.http.vo.PageReqVO;
import com.autel.cloud.ordercenter.vo.ClientOrderListReqVo;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @description
 * @auther A23204
 * @datetime 2024/5/29 9:28
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class SubscribeInfoPageVO extends PageReqVO<ClientOrderListReqVo> {
    @ApiModelProperty("排序字段")
    private String orderBy;

    @ApiModelProperty("排序类型")
    private String orderType;
}
