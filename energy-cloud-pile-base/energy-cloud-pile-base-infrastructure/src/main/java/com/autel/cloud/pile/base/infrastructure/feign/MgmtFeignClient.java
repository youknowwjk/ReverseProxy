package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiOperation;
import lombok.Data;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.io.Serializable;
import java.util.List;

@FeignClient(name = "ops-mgmt", contextId = "MgmtFeignClient")
public interface MgmtFeignClient {


    @ApiOperation(value = "同步广告添加设备到统一桩管理", notes = "同步广告添加设备到统一桩管理", httpMethod = "POST")
    @PostMapping("/pile/history")
    public Result<Page<PileInfoPO>> history(@RequestBody PileHistoryDTO pileGroupDTO);

    @Data
    public static class PileHistoryDTO extends PageDTO {

        private String sn;
        private Long  sellerId;

    }

    @Data
    public class PileInfoPO implements Serializable {

        private static final long serialVersionUID = 1L;

        /**
         * 主键
         */
        @TableId(type = IdType.ASSIGN_ID)
        @JsonFormat(shape = JsonFormat.Shape.STRING)
        private Long id;

        /**
         * SN编号
         */
        @TableField("sn")
        private String sn;

        /**
         * 运维商家编号
         */
        @TableField(value = "merchant_id", updateStrategy = FieldStrategy.IGNORED)
        private String merchantId;

        /**
         * 运营商家编号
         */
        @TableField(value = "operation_id", updateStrategy = FieldStrategy.IGNORED)
        private String operationId;

        /**
         * TCU-SN编号
         */
        @TableField("tcu_sn")
        private String tcuSn;

        /**
         * PIN码
         */
        @TableField("pin")
        private String pin;

        /**
         * 产品型号
         */
        @TableField("model")
        private String model;

        /**
         * 地址
         */
        @TableField("address")
        private String address;

        /**
         * 经度
         */
        @TableField("longitude")
        private String longitude;

        /**
         * 纬度
         */
        @TableField("latitude")
        private String latitude;

        /**
         * 品牌
         */
        @TableField("brand")
        private String brand;

        /**
         * 桩类型 1：交流 2：直流 3 交直流
         */
        @TableField("category")
        private Integer category;

        /**
         * 额定功率
         */
        @TableField("rated_power")
        private Double ratedPower;

        /**
         * MAC地址
         */
        @TableField("mac")
        private String mac;

        /**
         * 业务属性  1:商桩 2:家桩
         */
        @TableField(value = "business_attributes", updateStrategy = FieldStrategy.IGNORED)
        private Integer businessAttributes;

        /**
         * 绑定状态
         */
        @TableField("binding_state")
        private Boolean bindingState;

        /**
         * 绑定者账户
         */
        @TableField("binder_account")
        private Long binderAccount;

        /**
         * 枪类型
         */
        @TableField("connector_type")
        private String connectorType;

        /**
         * 枪数量
         */
        @TableField("connector_num")
        private Integer connectorNum;

        /**
         * 网络连接模式
         */
        @TableField("networking_mode")
        private Integer networkingMode;

        /**
         * 充电电流
         */
        @TableField("recharging_current")
        private Double rechargingCurrent;

        /**
         * 供电相数
         */
        @TableField("power_supply_phases")
        private Integer powerSupplyPhases;

        /**
         * 蓝牙mac地址
         */
        @TableField("bluetooth_address")
        private String bluetoothAddress;

        /**
         * OCPP协议版本
         */
        @TableField("ocpp_protocol_version")
        private String ocppProtocolVersion; //null

        /**
         * 以太网mac地址
         */
        @TableField("lan_mac")
        private String lanMac;

        /**
         * 创建时间
         */
        @TableField("create_time")
        private Long createTime;

        /**
         * 最后一次更新时间
         */
        @TableField("update_time")
        private Long updateTime;

        @TableField("pile_name")
        private String pileName;

        /**
         * 桩所在国家
         */
        @TableField("country")
        private String country;

        @TableField("state")
        private String state;

        @TableField("city")
        private String city;

        @TableField("zip_code")
        private String zipCode;

        @TableField("customer")
        private String customer;

        @TableField("contact_name")
        private String contactName;

        @TableField("contact_code")
        private String contactCode;

        @TableField("contact_number")
        private String contactNumber;

        @TableField("vendor")
        private String vendor;

        @TableField("pile_group_id")
        private Long pileGroupId;


        /**
         * 桩逻辑删除，对运维不可见，对Autel可见
         */
        @TableField("delete_status")
        private Integer deleteStatus;

        /**
         * 日志级别
         */
        @TableField("log_level")
        private String logLevel;


        @TableField("model_name")
        private String modelName;

        @TableField("zone_id")
        private String zoneId;

        @TableField(exist = false)
        private List<PileGunPO> pileGunPOList;

    }

    @Data
    class PileGunPO implements Serializable {

        private static final long serialVersionUID=1L;

        /**
         * 主键
         */
        @TableId(value = "id")
        private Long id;

        /**
         * 充电桩id，关联pile_info表
         */
        @TableField("pile_info_id")
        private Long pileInfoId;

        /**
         * 枪号
         */
        @TableField("gun_no")
        private Integer gunNo;

        /**
         * 枪类型，同运营平台
         */
        @TableField("gun_type")
        private Integer gunType;

    }

}
