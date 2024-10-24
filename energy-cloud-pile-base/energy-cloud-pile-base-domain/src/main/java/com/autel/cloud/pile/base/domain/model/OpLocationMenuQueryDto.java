package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.base.common.page.PageDTO;
import io.swagger.annotations.ApiModel;
import lombok.Data;

/**
 * @description
 * @auther A23204
 * @datetime 2024/5/28 10:09
 */
@ApiModel("场站下拉菜单分页查询请求参数")
@Data
public class OpLocationMenuQueryDto extends PageDTO {

    private Boolean queryBySeller;
}
