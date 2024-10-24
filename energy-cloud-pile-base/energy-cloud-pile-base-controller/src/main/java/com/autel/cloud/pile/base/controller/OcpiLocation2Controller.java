package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OcpiLocation2Service;
import com.autel.cloud.pile.base.vo.ocpi.v2.OcpiV2LocationVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * Author:   A19011
 * Description: LocationController
 * Date:     2022/2/21 21:08
 *
 * @Version 0.0.1-SNAPSHOT
 */
@RestController
@RequestMapping("/ocpi/2.2.1/location")
@Api(value = "OcpiLocation2Controller", tags = "OCPI2.2.1站点管理")
@Log4j2
@Validated
public class OcpiLocation2Controller {
    @Autowired
    OcpiLocation2Service ocpiLocationService;

    @GetMapping(value = "/locationList2")
    @ApiOperation(value = "ocpi2.2.1")
    public  ResponseEntity<List<OcpiV2LocationVO>> cpoLocationList(@RequestParam(name = "date_from", required = false) String dateFrom,
                                                                    @RequestParam(name = "date_to", required = false) String dateTo,
                                                                    @RequestParam(name = "offset", required = false) Integer offset,
                                                                    @RequestParam(name = "limit", required = false) Integer limit,
                                                                    @RequestHeader(name = "locationLink") String locationLink,
                                                                    @RequestParam(name = "cpoId") String cpoId){
        return ocpiLocationService.getLocationList(dateFrom, dateTo, offset, limit, locationLink, cpoId);
    }

    @GetMapping("/{location_id}/{evse_uid}")
    public Result<OcpiV2LocationVO> getEvseLocation(@PathVariable("location_id") String locationId,
                                                  @PathVariable("evse_uid") String evseUid){
        log.info("OcpiLocation2Controller getEvseLocation location_id:{}, evse_uid:{}",locationId, evseUid );
        return Result.ofSucceed(ocpiLocationService.getEvseLocation(locationId,evseUid));
    }

    @GetMapping(value = "/getOcpiLocationById")
    public Result<OcpiV2LocationVO> getOcpiLocationById(@RequestParam("location_id") String locationId){
        log.info("OcpiLocation2Controller getOcpiLocationById location_id:{}",locationId);
        return Result.ofSucceed(ocpiLocationService.getEvseLocation(locationId, null));
    }

}
