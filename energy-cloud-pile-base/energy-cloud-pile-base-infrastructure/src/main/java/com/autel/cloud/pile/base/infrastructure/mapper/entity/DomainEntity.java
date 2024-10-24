package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author A20019
 * @since 2022/3/21 10:28
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@TableName("t_domain")
public class DomainEntity {

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    /*域名路径*/
    private String domainUrl;

    /*logo路径*/
    private String logoPath;
}
