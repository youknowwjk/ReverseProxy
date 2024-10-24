package com.autel.cloud.pile.base.infrastructure.feign.dto;

import com.autel.cloud.pile.base.dto.OffPeakHourPeriodDTO;
import com.autel.cloud.pile.base.dto.PileConfigDTO;
import lombok.Data;

import java.util.List;

@Data
public class PileOffPeakHourParamDTO extends PileConfigDTO {

    private Boolean alertEnabled;

    private Integer alertTimeout;

    private String recurrencyKind;

    private List<OffPeakHourPeriodDTO> offPeakHours;

}
