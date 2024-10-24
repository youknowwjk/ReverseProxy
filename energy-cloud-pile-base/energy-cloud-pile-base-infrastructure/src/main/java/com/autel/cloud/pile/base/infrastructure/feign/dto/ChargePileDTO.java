package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * <p>Describe Class...</p>
 *
 * @author A19284
 * @since 2021/12/4 10:38
 */
@Data
@ApiModel(value = "ProductInfoDTO", description = "充电桩产品基础信息")
public class ChargePileDTO implements Serializable {
    private static final long serialVersionUID = 6239179628087263899L;

    /**
     * 产品(桩)ID
     */
    private String id;

    /**
     * 充电桩序列号
     */
    private String sn;

    /**
     * 充电桩pin码
     */
    private String pin;

    /**
     * 安装方式W：壁挂式 P：便携式 S：落地式
     */
    private String installWay;

    /**
     * 使用场景 1：商桩  2：家桩
     */
    private Integer usageScenario;

    /**
     * 产品类型 1：交流 1：直流 2 交直流
     */
    private Integer category;

    /**
     * 执行标准
     */
    private String standard;

    /**
     * 充电枪类型：C：Cable S：Socket H：Shutter
     */
    private Integer connectorType;

    /**
     * 枪数量
     */
    private Integer connectorNum;

    /**
     * 充电枪线长,单位为米
     */
    private Integer cableLength;

    /**
     * 输出功率,单位为KW
     */
    private Double outputPower;

    /**
     * 电压
     */
    private Double voltage;

    /**
     * n相电
     */
    private Integer phase;

    /**
     * 电流
     */
    private Double electricCurrent;

    /**
     * 连接方式
     */
    private String connectionMode;

    /**
     * 屏幕类型 L：LCD
     */
    private String screen;

    /**
     * 认证类型
     */
    private String certification;

    /**
     * 充电桩颜色：GR DG WH GD BL RG SV CG
     * Green、Dark Grey、White、Golden、Blue、Rose Gold、Silver、Champaign Gold
     */
    private String color;

    /**
     * 是否sub-ghz无线通讯
     */
    private Integer subGhz;

    /**
     * 固件控制板版本
     */
    private String firmwareEccVersion;

    /**
     * 固件功率板版本
     */
    private String firmwareEcpVersion;

    /**
     * 固件VCI版本
     */
    private String firmwareVciVersion;

    /**
     * 功率输出描述
     */
    private String powerOutputDesc;

    /**
     * 充电桩产品型号
     */
    private String productModel;

    /**
     * 制造商
     */
    private String manufacturer;

    /**
     * 经销商编号
     */
    private String sealerNo;

    @ApiModelProperty(value = "产品网站桩产品类型")
    private String productType;

    /**
     * 品牌id
     */
    private String brandId;

    /**
     * 品牌名称
     */
    private String brandName;

    /**
     * 固件信息列表
     */
    private List<FirmwareVersionDTO> firmwareList;

    /**
     * 是否是第三方充电桩(true:是;false:否)
     */
    @ApiModelProperty(value = "是否是第三方充电桩(true:是;false:否)")
    private Boolean thirdPart;

    /**
     * 屏幕1尺寸
     */
    @ApiModelProperty(value = "屏幕1尺寸")
    private Integer screen1Size;

    /**
     * 机型
     */
    private String partProduct;

    /**
     * 系列号
     */
    private String productNamePdm;

    /**
     * 屏幕1分辨率
     */
    @ApiModelProperty(value = "屏幕1分辨率")
    private Integer screen1Pixel;

    private Integer internalSerialName;
}
