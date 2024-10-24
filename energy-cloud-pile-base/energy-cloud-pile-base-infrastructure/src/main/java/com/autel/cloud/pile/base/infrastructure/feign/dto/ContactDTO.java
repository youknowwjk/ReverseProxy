package com.autel.cloud.pile.base.infrastructure.feign.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * <p>
 * 桩联系人
 * </p>
 *
 * @author William
 * @since 2022/6/25
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ContactDTO implements Serializable {

    /**
     * 联系人姓名
     */
    private String contactName;

    /**
     * 手机号国家编码
     */
    private String countryCode;

    /**
     * 手机号
     */
    private String mobilePhone;

}
