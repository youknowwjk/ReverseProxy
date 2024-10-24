package com.autel.cloud.pile.base.infrastructure.feign.dto;

import lombok.Data;

@Data
public class LastExpireBindRelation {

    private String pileSn;

    private String serviceId;

    private Long tenantId;

    private Long unavailableTime;
}