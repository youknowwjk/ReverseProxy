package com.autel.cloud.pile.base.infrastructure.feign.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

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
@ApiModel(value = "PileInfoDTO", description = "充电桩DTO")
@Builder
public class PileInfoDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @ApiModelProperty("id")
    private String id;

    /**
     * SN编号
     */
    @ApiModelProperty("SN编号")
    private String sn;

    /**
     * PIN码
     */
    @ApiModelProperty("PIN码")
    private String pin;

    /**
     * 地址
     */
    @ApiModelProperty("地址")
    private String address;

    /**
     * 经度
     */
    @ApiModelProperty("经度")
    private String longitude;

    /**
     * 纬度
     */
    @ApiModelProperty("纬度")
    private String latitude;

    /**
     * 品牌
     */
    @ApiModelProperty("品牌")
    private String brand;

    /**
     * 桩类型 0：交流 1：直流 2 交直流
     */
    @ApiModelProperty("桩类型 0：交流 1：直流 2 交直流")
    private Integer category;

    /**
     * 额定功率
     */
    @ApiModelProperty("额定功率")
    private String ratedPower;

    /**
     * 供电相数
     */
    @ApiModelProperty("供电相数")
    private Integer powerSupplyPhases;

    /**
     * 充电桩名称
     */
    @ApiModelProperty("充电桩名称")
    private String pileName;

    /**
     * 充电桩所在国家
     */
    @ApiModelProperty("充电桩所在国家")
    private String country;

    /**
     * 充电桩所在州
     */
    @ApiModelProperty("充电桩所在州")
    private String pileState;

    /**
     * 充电桩所在城市
     */
    @ApiModelProperty("充电桩所在城市")
    private String city;

    /**
     * 邮政编码
     */
    @ApiModelProperty("邮政编码")
    private String zipCode;

    /**
     * 客户名称
     */
    @ApiModelProperty("客户名称")
    private String customer;

    /**
     * 分组id
     */
    @ApiModelProperty("分组id")
    private String pileGroupId;

    /**
     * 场站所在组织id
     */
    @ApiModelProperty("场站所在组织id")
    private String groupId;

    /**
     * 充电枪列表
     */
    @ApiModelProperty("充电枪列表")
    List<ConnectorDTO> connectorList;

    /**
     * 联系人列表
     */
    @ApiModelProperty("联系人列表")
    List<ContactDTO> contactList;

    /**
     * 产品型号
     */
    @ApiModelProperty("产品型号")
    private String model;

    /**
     * 供应商
     */
    @ApiModelProperty("供应商")
    private String vendor;

    /**
     * 拥有关系商户ID
     */
    @ApiModelProperty("拥有关系商户ID")
    private String merchantId;

    /**
     * 运维关系的商户ID
     */
    @ApiModelProperty("运维关系的商户ID")
    private String operationId;
}
