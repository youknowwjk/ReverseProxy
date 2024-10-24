package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseReservationService;
import com.autel.cloud.pile.base.dto.ReservationDetailDTO;
import com.autel.cloud.pile.base.vo.ReservationDetailVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * @author A22587
 */
@Log4j2
@RestController
@RequestMapping("/reservation")
@Api(tags = "预约充电")
@Validated
public class OpLocationEvseReservationController {

    @Resource
    private OpLocationEvseReservationService opLocationEvseReservationService;

    @PostMapping("/informationDisplay")
    @ApiOperation(value = "预约页面信息展示", notes = "预约页面信息展示")
    public Result<ReservationDetailVO> reservationInformationDisplay(@Valid @RequestBody ReservationDetailDTO reservationDetailDTO) {

        log.info("reservationDetailDTO:{}", JSON.toJSONString(reservationDetailDTO));

        Result<ReservationDetailVO> result = opLocationEvseReservationService.reservationInformationDisplay(reservationDetailDTO);

        log.info("reservationInformationDisplay.result:{}", JSON.toJSONString(result));

        return result;
    }
}
