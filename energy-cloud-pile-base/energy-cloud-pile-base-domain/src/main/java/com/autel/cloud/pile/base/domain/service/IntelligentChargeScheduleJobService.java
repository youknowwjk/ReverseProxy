package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.domain.data.SmartChargeJobQuery;
import com.autel.cloud.pile.bill.dto.IntelligentChargeVO;
import org.springframework.web.bind.annotation.RequestBody;

public interface IntelligentChargeScheduleJobService {
    IntelligentChargeVO jobStatus(@RequestBody SmartChargeJobQuery smartChargeJobQuery);
}
