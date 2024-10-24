package com.autel.cloud.pile.base.infrastructure.feign.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @author Zoe Liu
 * @data 2022/3/14
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AddNewPileDTO implements Serializable {

    private String sn;

    private String pin;

    private String brand;

    private String gunTypeId;

    private String address;

    private String longitude;

    private String latitude;

    private String merchantId;
}
