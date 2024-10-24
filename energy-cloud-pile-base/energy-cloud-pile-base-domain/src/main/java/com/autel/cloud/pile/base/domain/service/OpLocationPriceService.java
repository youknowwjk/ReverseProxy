package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.LocationPriceDTO;
import com.autel.cloud.pile.base.vo.OpLocationPriceInfoVo;
import com.autel.cloud.pile.base.vo.OpLocationPriceVO;

import java.util.List;

/**
 * @author temp
 * @description 针对表【op_location_price(成本电价表)】的数据库操作Service
 * @createDate 2023-03-07 14:48:13
 */
public interface OpLocationPriceService {

    Long add(LocationPriceDTO dto);

    Long edit(LocationPriceDTO dto);

    OpLocationPriceVO detail(Long id);

    Long delete(Long id);

    List<OpLocationPriceVO> getList(Long locationId);

    int deleteByLocationId(Long id);

    OpLocationPriceInfoVo getPriceList(Long locationId);
}
