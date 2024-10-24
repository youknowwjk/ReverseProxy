package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.vo.ocpi.v2.OcpiV2LocationVO;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Author:   A19011
 * Description: OcpiLocationService
 * Date:     2022/6/20 15:33
 *
 * @Version 0.0.1-SNAPSHOT
 */
@Service
public interface OcpiLocation2Service {

    ResponseEntity<List<OcpiV2LocationVO>> getLocationList(String dateFrom, String dateTo, Integer offset, Integer limit, String locationLink, String cpoId);

    OcpiV2LocationVO getEvseLocation(String locationId, String evseUid);
}
