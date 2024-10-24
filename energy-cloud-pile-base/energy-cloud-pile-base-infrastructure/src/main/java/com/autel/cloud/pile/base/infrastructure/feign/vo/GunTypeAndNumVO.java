package com.autel.cloud.pile.base.infrastructure.feign.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class GunTypeAndNumVO {
    private String pileSn;
    private String gunNum;
    private Map<String,String> gunTypeMap;
}
