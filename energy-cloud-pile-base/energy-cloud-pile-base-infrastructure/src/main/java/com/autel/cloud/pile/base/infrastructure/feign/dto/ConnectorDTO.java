package com.autel.cloud.pile.base.infrastructure.feign.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ConnectorDTO implements Serializable {

    /**
     * 枪号
     */
    private Integer connectorNo;

    /**
     * 枪类型
     */
    private Integer connectorType;
}
