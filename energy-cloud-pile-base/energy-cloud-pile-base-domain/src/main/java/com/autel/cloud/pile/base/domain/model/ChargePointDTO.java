package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.enums.MerchantType;
import com.autel.cloud.pile.base.vo.Connector;
import com.autel.cloud.pile.base.vo.Terminal;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
@ApiModel
public class ChargePointDTO {

    /**
     * 桩类型 1：交流， 2：直流， 3：交直流
     */
//    @ApiModelProperty("桩类型 1：交流 2：直流 3 交直流")
//    private int category;

    @ApiModelProperty(value = "商家类型", hidden = true)
    private MerchantType merchantType;


    /**
     * 品牌id
     */
    @ApiModelProperty("品牌id")
    private Long brandId;

    /**
     * 品牌
     */
    @ApiModelProperty("品牌")
    private String brandName;

    /**
     * 桩名称
     */
    @ApiModelProperty("桩名称")
    private String name;

    @ApiModelProperty(value = "运维商桩命名", hidden = true)
    private String name_1;

    @ApiModelProperty(value = "经销商桩命名", hidden = true)
    private String name_2;

    @ApiModelProperty(value = "CPO桩命名", hidden = true)
    private String name_3;

    /**
     * 桩所属cpo商家id
     */
    @ApiModelProperty(value = "桩所属cpo商家id", hidden = true)
    private Long owner;

    /**
     * cpo绑定时间
     */
    @ApiModelProperty(value = "cpo绑定时间", hidden = true)
    private Long ownerBindTime;

    /**
     * 桩运维管理商家id
     */
    @ApiModelProperty(value = "桩运维管理商家id", hidden = true)
    private Long maintenance;

    /**
     * 运维绑定时间
     */
    @ApiModelProperty(value = "运维绑定时间", hidden = true)
    private Long maintenanceBindTime;

    /**
     * sn编号
     */
    @ApiModelProperty("桩sn编号")
    private String sn;

    /**
     * pin码
     */
    @ApiModelProperty("桩pin码")
    private String pin;


    @ApiModelProperty("产品系列")
    private String productNamePdm;

    @ApiModelProperty("产品型号")
    private String partProduct;


    @ApiModelProperty(value = "时区", required = false)
    private String zoneId;


    /**
     * 供电相数
     */
    @ApiModelProperty("供电相数")
    private String phases;


    /**
     * AC  DC
     */
    @ApiModelProperty("AC  DC ")
    private String powerType;

    /**
     * 额定功率
     */
    @ApiModelProperty("额定功率")
    private BigDecimal ratedPower;


    @ApiModelProperty("枪类型列表")
    private List<Connector> connectors;

    /**
     * @see com.autel.cloud.pile.base.enums.chargepoint.OverchargingPileFlagEnum
     */
    @ApiModelProperty("超充桩标志(0:非超充桩; 1:超充桩)")
    private Integer overchargingPileFlag;

    @ApiModelProperty("超充桩终端信息")
    private List<Terminal> terminals;
}
