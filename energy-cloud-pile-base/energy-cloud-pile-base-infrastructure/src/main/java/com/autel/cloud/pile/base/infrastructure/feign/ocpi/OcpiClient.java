package com.autel.cloud.pile.base.infrastructure.feign.ocpi;

import com.autel.cloud.ocpi.vo.location.EVSEVO;
import com.autel.cloud.pile.base.vo.OcpiLocationVO;
import com.autel.cloud.tariff.vo.ocpi.TariffVO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * Author:   A19011
 * Description: OcpiClient
 * Date:     2022/3/15 21:45
 *
 * @Version 0.0.1-SNAPSHOT
 */
@FeignClient(value = "ocpi")
public interface OcpiClient {
    @PutMapping("/iop/cpo/2.1.1/locations/patch/{location_id}")
    Object updateLocation(@PathVariable("location_id") String location_id,
                          @RequestBody OcpiLocationVO locationVO);

    @PutMapping("/iop/cpo/2.1.1/locations/patchLocation/{location_id}")
    Object patchLocation(@PathVariable("location_id") String location_id,
                         @RequestParam(value = "cpo_id", required = false) String cpo_id,
                         @RequestBody OcpiLocationVO locationVO);

    @PutMapping("/iop/cpo/2.1.1/tariffs/{tariff_id}")
    @ApiOperation(value = "新增/更新Tariff")
    OcpiCloudResponse<Object> saveUpdateTariff(@PathVariable("tariff_id") String tariff_id,
                                               @RequestBody TariffVO tariffVO);


    @PutMapping("/iop/cpo/2.1.1/updateTariffs/{tariff_id}")
    @ApiOperation(value = "新增/更新Tariff")
    OcpiCloudResponse<Object> saveUpdateTariff(@PathVariable("tariff_id") String tariff_id,
                                               @RequestParam(value = "cpo_id", required = false) String cpo_id,
                                               @RequestBody TariffVO tariffVO);

    @ApiOperation(value = "向IOP推送EVSE变更")
    @PutMapping("/iop/cpo/2.1.1/locations/{location_id}/{evse_uid}")
    OcpiCloudResponse<Boolean> patchToIOPEvse(@PathVariable("location_id") String location_id,
                                              @PathVariable("evse_uid") String evse_uid,
                                              @RequestParam(value = "cpo_id", required = false) String cpo_id,
                                              @RequestBody EVSEVO evseVO);
}
