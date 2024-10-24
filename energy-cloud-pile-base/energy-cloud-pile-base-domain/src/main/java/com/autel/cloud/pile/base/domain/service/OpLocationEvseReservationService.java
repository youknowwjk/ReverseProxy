package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.ReservationDetailDTO;
import com.autel.cloud.pile.base.vo.ReservationDetailVO;

/**
 * @author A22587
 */
public interface OpLocationEvseReservationService {

    Result<ReservationDetailVO> reservationInformationDisplay(ReservationDetailDTO reservationDetailDTO);

}
