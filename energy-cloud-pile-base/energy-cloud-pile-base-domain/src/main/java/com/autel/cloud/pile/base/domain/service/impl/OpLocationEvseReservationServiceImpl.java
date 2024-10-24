package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseReservationRepository;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseReservationService;
import com.autel.cloud.pile.base.dto.ReservationDetailDTO;
import com.autel.cloud.pile.base.vo.ReservationDetailVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * @author A22587
 */
@Service
@Slf4j
public class OpLocationEvseReservationServiceImpl implements OpLocationEvseReservationService {

    @Resource
    private OpLocationEvseReservationRepository opLocationEvseReservationRepository;

    @Override
    public Result<ReservationDetailVO> reservationInformationDisplay(ReservationDetailDTO reservationDetailDTO) {

        return opLocationEvseReservationRepository.reservationInformationDisplay(reservationDetailDTO);

    }
}
