package com.autel.cloud.pile.base.domain.convert;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationTempEntity;

import java.util.List;
import java.util.stream.Collectors;

public class OpLocationTempConvert {

    private OpLocationTempConvert() {

    }

    /**
     * to OpLocationForNewDTO
     *
     * @param opLocationTempEntity
     * @return
     */
    public static OpLocationForNewDTO toOpLocationForNewDTO(OpLocationTempEntity opLocationTempEntity) {
        if (opLocationTempEntity == null) {
            return null;
        }
        OpLocationForNewDTO opLocationForNewDTO = new OpLocationForNewDTO();
        if (StrUtil.isNotBlank(opLocationTempEntity.getLocationJson())) {
            opLocationForNewDTO.setOpLocationDTO(JSON.parseObject(opLocationTempEntity.getLocationJson(), OpLocationDTO.class));
            //成本电价排序
            OpLocationDTO opLocationDTO = opLocationForNewDTO.getOpLocationDTO();
            LocationPriceDTO locationPriceDTO = opLocationDTO.getLocationPriceDTO();
            if (locationPriceDTO != null) {
                List<LocationPriceDetailDTO> priceDetails = locationPriceDTO.getPriceDetails();
                List<LocationPriceDetailDTO> sortList = priceDetails.stream().sorted((f, s) -> (int) (f.getSort() - s.getSort())).collect(Collectors.toList());
                locationPriceDTO.setPriceDetails(sortList);
            }
        }
        if (StrUtil.isNotBlank(opLocationTempEntity.getPileJson())) {
            opLocationForNewDTO.setOpLocationEvseDTOs(JSON.parseArray(opLocationTempEntity.getPileJson(), OpLocationEvseDTO.class));
        }
        if (StrUtil.isNotBlank(opLocationTempEntity.getPileTariffJson())) {
            opLocationForNewDTO.setPileTariffList(JSON.parseArray(opLocationTempEntity.getPileTariffJson(), PileTariffMapDTO.class));
        }
        opLocationForNewDTO.setTariffId(opLocationTempEntity.getTariffId());
        return opLocationForNewDTO;
    }

}
