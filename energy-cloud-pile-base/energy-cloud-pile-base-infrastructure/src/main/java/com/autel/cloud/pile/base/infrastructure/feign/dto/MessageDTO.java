package com.autel.cloud.pile.base.infrastructure.feign.dto;

import com.alibaba.fastjson.JSON;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author 吴超
 * @date 2021/5/20 14:44
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class MessageDTO<T> {
    private String cmd;
    private String seq;
    private int code;
    private String message;
    private T data;

    @Override
    public String toString() {
        return JSON.toJSONString(this);
    }


    /**
     * 消息指令枚举类
     */
    public enum CmdEnum {
        SMART_CHARGE_SCHEDULE("smartChargeSchedule", "SMART_CHARGE_SCHEDULE"),
        SOC_DISPLAY("socDisplay", "SOC_DISPLAY"),
        /**
         * bms数据
         */
        BMS_INFO("bmsInfo", "BMS_INFO"),
        REFRESH_CONFIG("refreshConfig", "REFRESH_CONFIG"),
        STRIPE_BIND_INFO("stripeBindInfo", "STRIPE_BIND_INFO"),
        GET_CONFIGURATION("getConfiguration","GetConfiguration");


        private String key;
        private String value;

        CmdEnum(String key, String value) {
            this.setKey(key);
            this.setValue(value);
        }

        public String getKey() {
            return key;
        }

        public void setKey(String key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }

}
