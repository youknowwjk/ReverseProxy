package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.autel.cloud.pile.base.enums.MerchantChargePointRelationEnum;
import com.autel.cloud.pile.base.enums.SubStatus;
import com.autel.cloud.pile.base.vo.Connector;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.handlers.FastjsonTypeHandler;
import lombok.Data;

import java.util.List;

/**
 * 商家资产 桩资产信息表
 */
@Data
@TableName(value = "charge_point_merchant_relation", autoResultMap = true)
public class ChargePointMerchantRelationEntity {

    @TableId(type = IdType.ASSIGN_ID)
    /**
     * id
     */
    private Long id;

    /**
     * 品牌id
     */
    private Long brandId;

    /**
     * 品牌
     */
    private String brandName;

    /**
     * 桩名称
     */
    private String name;

    /**
     * sn编号
     */
    private String sn;

    /**
     * pin码
     */
    private String pin;

    /**
     * 桩所属商家id
     */
    private Long merchantId;

    /**
     * 商家绑定时间
     */
    private Long bindTime;

    /**
     * 订阅状态
     * @see MerchantChargePointRelationEnum
     */
    private int relation;

    /**
     * 订阅状态
     * @see SubStatus
     */
    private int subStatus;

    /**
     *
     */
    private Long subExpire;

    /**
     * 系列号
     */
    private String productNamePdm;

    /**
     * 产品型号
     */
    private String partProduct;

    /**
     * 桩类型 1：交流， 2：直流， 3：交直流
     */
    private Integer category;

    /**
     * 额定功率
     */
    private Double ratedPower;

    /**
     * mac地址
     */
    private String mac;

    /**
     * dc; ac
     */
    private String powerType;

    /**
     * 供电相数
     */
    private String phases;

    /**
     * 屏幕1尺寸
     */
    private Integer screen1Size;

    /**
     * 屏幕1分辨率
     */
    private Integer screen1Pixel;

    /**
     * 创建时间
     */
    private Long createTime;

    /**
     * 更新时间
     */
    private Long updateTime;

    /**
     * 时区id名称
     */
    private String zoneId;

    /**
     * 充电枪
     */
    @TableField(typeHandler = FastjsonTypeHandler.class)
    private List<Connector> connectors;

    @TableField(exist = false)
    private String connectorsString;

    @TableField(exist = false)
    private String locationName;

    /**
     * @see com.autel.cloud.pile.base.enums.chargepoint.OverchargingPileFlagEnum
     */
    private Integer overchargingPileFlag;
}
