package com.autel.cloud.pile.base.domain.repository;


import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.ReservationDetailDTO;
import com.autel.cloud.pile.base.vo.ReservationDetailVO;

/**
 * @author A22587
 */
public interface OpLocationEvseReservationRepository {


    Result<ReservationDetailVO> reservationInformationDisplay(ReservationDetailDTO reservationDetailDTO);

}
