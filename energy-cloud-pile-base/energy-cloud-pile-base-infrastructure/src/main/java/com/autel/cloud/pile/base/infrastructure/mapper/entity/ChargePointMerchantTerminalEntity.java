package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.enums.SubStatus;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;

import com.autel.cloud.pile.base.vo.Connector;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;

import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.handlers.FastjsonTypeHandler;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Collections;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

@Data
@TableName(value = "charge_point_merchant_terminal", autoResultMap = true)
@ApiModel(description = "商家终端资产表")
public class ChargePointMerchantTerminalEntity {

    @TableId(value = "id", type = IdType.ASSIGN_ID)
    @ApiModelProperty("主键id")
    private Long id;

    @ApiModelProperty("终端SN")
    @TableField(value = "terminal_sn")
    private String terminalSn;

    @ApiModelProperty("终端名称")
    @TableField(value = "terminal_name")
    private String terminalName;

    @ApiModelProperty("终端PIN码")
    @TableField(value = "terminal_pin")
    private String terminalPin;

    /**
     *  类型: List<Connector>
     * @see com.autel.cloud.pile.base.vo.Connector
     */
    @ApiModelProperty("终端枪类型列表")
    @TableField(value = "connectors")
    private String connectors;

    @ApiModelProperty("主机SN")
    @TableField(value = "host_sn")
    private String hostSn;

    @ApiModelProperty("商家id")
    @TableField(value = "merchant_id")
    private Long merchantId;
    /**
     * 订阅状态
     * @see SubStatus
     */
    @TableField(value = "sub_status")
    private Integer subStatus;

    @ApiModelProperty("创建时间")
    @TableField(value = "create_time")
    private Long createTime;

    @ApiModelProperty("更新时间")
    @TableField(value = "update_time")
    private Long updateTime;

    public List<Connector> getConnectorsList() {
        if (StringUtils.isBlank(this.connectors)) {
            return Collections.emptyList();
        }
        List<Connector> connectors = JSON.parseArray(this.connectors, Connector.class);
        Collections.sort(connectors);
        return connectors;
    }

    public void setJsonFormatConnectors(List<Connector> connectors) {
        if (ObjectUtils.isEmpty(connectors)) {
            this.connectors = null;
        }

        Collections.sort(connectors);

        this.connectors = JSON.toJSONString(connectors);
    }
}
