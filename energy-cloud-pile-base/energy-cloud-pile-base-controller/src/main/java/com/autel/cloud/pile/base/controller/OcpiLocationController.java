package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OcpiLocationService;
import com.autel.cloud.pile.base.vo.EvseDynamicPricingVO;
import com.autel.cloud.pile.base.vo.OcpiLocationVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiModelProperty;
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
@RequestMapping("/ocpi/location")
@Api(value = "OcpiLocationController", tags = "OCPI站点管理")
@Log4j2
@Validated
public class OcpiLocationController {
    @Autowired
    OcpiLocationService ocpiLocationService;

    @GetMapping("/list")
    public ResponseEntity<List<OcpiLocationVO>> getLocationList(@RequestParam(name = "date_from", required = false) String dateFrom,
                                                                @RequestParam(name = "date_to", required = false) String dateTo,
                                                                @RequestParam(name = "offset", required = false, defaultValue = "0") Integer offset,
                                                                @RequestParam(name = "limit", required = false, defaultValue = "10") Integer limit,
                                                                @RequestHeader(name = "locationLink", required = true) String locationLink){
        return ocpiLocationService.getLocationList(dateFrom, dateTo, offset, limit, locationLink,"");
    }

    @GetMapping(value = "/locationList")
    public  ResponseEntity<List<OcpiLocationVO>> cpoLocationList(@RequestParam(name = "date_from", required = false) String dateFrom,
                                                  @RequestParam(name = "date_to", required = false) String dateTo,
                                                  @RequestParam(name = "offset", required = false) Integer offset,
                                                  @RequestParam(name = "limit", required = false) Integer limit,
                                                  @RequestHeader(name = "locationLink") String locationLink,
                                                  @RequestParam(name = "cpoId") String cpoId){
        return ocpiLocationService.getLocationList(dateFrom, dateTo, offset, limit, locationLink, cpoId);
    }


    @GetMapping("/{location_id}/{evse_uid}")
    public Result<OcpiLocationVO> getEvseLocation(@PathVariable("location_id") String locationId,
                                                   @PathVariable("evse_uid") String evseUid){
        log.info("getEvseLocation location_id:{}, evse_uid:{}",locationId, evseUid );
        return Result.ofSucceed(ocpiLocationService.getEvseLocation(locationId,evseUid));
    }


    @GetMapping(value = "/getOcpiLocationById")
    public Result<OcpiLocationVO> getOcpiLocationById(@RequestParam("location_id") String locationId){
        log.info("getOcpiLocationById location_id:{}",locationId);
        return Result.ofSucceed(ocpiLocationService.getEvseLocation(locationId, null));
    }

    @ApiModelProperty("场站ocpi启用设置")
    @PutMapping("/{location_id}/{ocpi_enabled}")
    public Result<Boolean> updateLocationOcpiEnalbed(@PathVariable("location_id") String locationId,
                                                  @PathVariable("ocpi_enabled") Boolean ocpiEnabled) {
        log.info("updateLocationOcpiEnalbed location_id:{}, ocpi_enabled:{}", locationId, ocpiEnabled) ;
        return Result.ofSucceed(ocpiLocationService.updateLocationOcpiEnalbed(locationId,ocpiEnabled));
    }

    @ApiModelProperty("场站是否OCPI启用")
    @GetMapping("/{location_id}")
    public Result<Boolean> queryLocationOcpiEnabled(@PathVariable("location_id") String locationId) {
        log.info("queryLocationOcpiEnabled location_id:{}", locationId) ;
        return Result.ofSucceed(ocpiLocationService.queryLocationOcpiEnabled(locationId));
    }

    @ApiModelProperty("计费规则是否OCPI启用")
    @GetMapping("/tariff/{tariff_id}")
    public Result<Boolean> queryTraiffOcpiEnabled(@PathVariable("tariff_id") String tariffId) {
        log.info("queryTraiffOcpiEnabled tariff_id:{}", tariffId) ;
        return Result.ofSucceed(ocpiLocationService.queryTariffOcpiEnabled(tariffId));
    }

    /**
     * @param pageDTO
     * @return
     * @function 提供清单页面供EMP拉取(OCPI Gireve)
     */
    @PostMapping("/page")
    @ApiOperation(value = "提供清单页面供EMP拉取(OCPI Gireve)", notes = "提供清单页面供EMP拉取(OCPI Gireve)")
    public Result<Page<EvseDynamicPricingVO>> page(@RequestBody PageDTO pageDTO) {

        log.info("=====>>>>>OcpiLocationController.page pageDTO : {}", JSON.toJSONString(pageDTO));

        return Result.ofSucceed(ocpiLocationService.page(pageDTO));
    }
}
