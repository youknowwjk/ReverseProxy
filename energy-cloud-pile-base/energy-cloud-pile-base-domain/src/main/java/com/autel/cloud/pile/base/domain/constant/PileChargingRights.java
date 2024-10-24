package com.autel.cloud.pile.base.domain.constant;

import com.autel.cloud.pile.base.enums.ServiceId;

import java.util.Arrays;
import java.util.List;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/27 16:48
 */
public class PileChargingRights {

    /**
     * 具有充电权益的rights 集合
     */
    public static final List<String> SERVICE_ID_LIST = Arrays.asList(ServiceId.SERVICE_ID_1, ServiceId.SERVICE_ID_2);

    public static final List<String> SERVICE_ID_LIST_ALL = Arrays.asList(ServiceId.SERVICE_ID_1, ServiceId.SERVICE_ID_3);

    /**
     * 业务改成从功能点开始
     */

    // 启动充电
    public static final String START_CHARGE = "001";

    // APP 地图搜索 004
    public static final String GUN_SEARCH = "004";

    // 查看桩详情
    public static final String PILE_DETAIL = "005";

    /**
     * 运营权益集合
     */
    public static final List<String> OPERATION_SERVICE_ID_LIST = Arrays.asList(ServiceId.SERVICE_ID_BENEFITS_PRO, ServiceId.SERVICE_ID_BENEFITS_LITE);

    /**
     * 运营+运维权益集合
     */
    public static final List<String> OPERATION_SERVICE_ID_LIST_ALL = Arrays.asList(ServiceId.SERVICE_ID_BENEFITS_PRO, ServiceId.SERVICE_ID_BENEFITS_OPS);

    /**
     * 在售商品集合
     */
    public static final List<String>  GOODS_NAME_LIST = Arrays.asList("Pro", "Lite", "Advertising", "Maintenance");

    public static final List<String> SERVICE_ID_ALL = Arrays.asList(ServiceId.SERVICE_ID_BENEFITS_PRO, ServiceId.SERVICE_ID_BENEFITS_LITE, ServiceId.SERVICE_ID_BENEFITS_ADS, ServiceId.SERVICE_ID_BENEFITS_OPS);

}
