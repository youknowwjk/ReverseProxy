package com.autel.cloud.pile.base.domain.constant;

import com.google.common.collect.Lists;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;

import java.util.List;


@Getter
public enum LocationEvseStatusMappingEnum {
    AVAILABLE(1, "Available", "空闲",3,"Available","空闲"),
    PREPARING(2, "Preparing", "准备中",2,"Occupied","占用"),
    CHARGING(3, "Charging", "充电中",1,"Charging","充电中"),
    FINISHING(4, "Finishing", "充电结束",2,"Occupied","占用"),
    SUSPENDED_EVSE(5, "SuspendedEVSE", "暂停充电设备",1,"Charging","充电中"),
    SUSPENDED_EV(6, "SuspendedEV", "暂停汽车",2,"Occupied","占用"),
    FAULTED(7, "Faulted", "故障",5,"Faulty","故障"),
    RESERVED(8, "Reserved", "预约",7,"Reserved","预约"),
    UNAVAILABLE(9, "Unavailable", "不可用，正在初始化 测试 维护 禁用",6,"In maintenance","不可用"),
    DEFAULT(255, "Default", "默认",7,"Offline","离线");

    private final Integer code;
    private final String name;
    private final String desc;

    private final Integer locationCode;
    private final String locationName;
    private final String locationDesc;

     LocationEvseStatusMappingEnum(Integer code, String name, String desc, Integer locationCode, String locationName, String locationDesc) {
        this.code = code;
        this.name = name;
        this.desc = desc;
        this.locationCode = locationCode;
        this.locationName = locationName;
        this.locationDesc = locationDesc;
    }

    public static LocationEvseStatusMappingEnum getLocationEvseStatueNameByEvseState(String name) {

        for (LocationEvseStatusMappingEnum value : LocationEvseStatusMappingEnum.values()) {
            if(StringUtils.equalsIgnoreCase( value.getName(),name)){
                return value;
            }
        }
        return DEFAULT;
    }

    public static List<LocationEvseStatusMappingEnum> getEvseStatueNameByLocationEvse(String name) {
         List<LocationEvseStatusMappingEnum> enumList = Lists.newArrayList();
        for (LocationEvseStatusMappingEnum value : LocationEvseStatusMappingEnum.values()) {
            if(StringUtils.equalsIgnoreCase( value.getLocationName(),name)){
                enumList.add(value);
            }
        }
        return enumList;
    }

}
