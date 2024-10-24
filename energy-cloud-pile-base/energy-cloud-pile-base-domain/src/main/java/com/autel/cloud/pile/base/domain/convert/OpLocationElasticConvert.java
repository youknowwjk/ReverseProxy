package com.autel.cloud.pile.base.domain.convert;


import com.autel.cloud.pile.base.dto.OpLocationDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationOperationEntity;
import org.springframework.beans.BeanUtils;
import org.springframework.data.elasticsearch.core.geo.GeoPoint;

public class OpLocationElasticConvert {

    private OpLocationElasticConvert() {

    }

    /**
     * 转换opLocationElasticDTO
     *
     * @param opLocationDTO
     * @return
     */
    public static OpLocationElasticDTO opLocationElasticDTO(OpLocationDTO opLocationDTO) {
        OpLocationElasticDTO opLocationElasticDTO = new OpLocationElasticDTO();
        opLocationElasticDTO.setAddress(opLocationDTO.getAddress());
        opLocationElasticDTO.setChargingWhenClosed(opLocationDTO.getChargingWhenClosed());
        opLocationElasticDTO.setCity(opLocationDTO.getCity());
        opLocationElasticDTO.setCountry(opLocationDTO.getCountry());
        opLocationElasticDTO.setLatitude(opLocationDTO.getLatitude());
        opLocationElasticDTO.setLongitude(opLocationDTO.getLongitude());
        opLocationElasticDTO.setName(opLocationDTO.getName());
        opLocationElasticDTO.setOperatorId(opLocationDTO.getOperatorId());
        opLocationElasticDTO.setOwnerId(opLocationDTO.getOwnerId());
        opLocationElasticDTO.setPostalCode(opLocationDTO.getPostalCode());
        opLocationElasticDTO.setStatus(opLocationDTO.getStatus());
        opLocationElasticDTO.setSubOperatorId(opLocationDTO.getSubOperatorId());
        opLocationElasticDTO.setTimeZone(opLocationDTO.getTimeZone());
        opLocationElasticDTO.setType(opLocationDTO.getType());
        opLocationElasticDTO.setOpenType(opLocationDTO.getOpenType());

        return opLocationElasticDTO;
    }

    /**
     * 转化
     *
     * @param opLocationEntity
     * @param opLocationOperationEntity
     * @return
     */
    public static OpLocationElasticDTO toOpLocationElasticDTO(OpLocationEntity opLocationEntity,
                                                              OpLocationOperationEntity opLocationOperationEntity) {
        OpLocationElasticDTO opLocationElasticDTO = new OpLocationElasticDTO();
        BeanUtils.copyProperties(opLocationOperationEntity, opLocationElasticDTO);
        opLocationElasticDTO.setAddress(opLocationEntity.getAddress());
        opLocationElasticDTO.setChargingWhenClosed(opLocationEntity.getChargingWhenClosed());
        opLocationElasticDTO.setCity(opLocationEntity.getCity());
        opLocationElasticDTO.setCountry(opLocationEntity.getCountry());
        opLocationElasticDTO.setProvince(opLocationEntity.getProvince());
        opLocationElasticDTO.setCreatedAt(opLocationEntity.getCreatedAt());
        GeoPoint location = new GeoPoint(Double.parseDouble(opLocationEntity.getLatitude()), Double.parseDouble(opLocationEntity.getLongitude()));
        opLocationElasticDTO.setLocation(location);
        opLocationElasticDTO.setHubjectCheck(opLocationEntity.getHubjectCheck());
        opLocationElasticDTO.setHouseNumber(opLocationEntity.getHouseNumber());
        opLocationElasticDTO.setReservationEnabled(opLocationEntity.getReservationEnabled());
        opLocationElasticDTO.setPlatform(1);
        opLocationElasticDTO.setLatitude(opLocationEntity.getLatitude());
        opLocationElasticDTO.setLongitude(opLocationEntity.getLongitude());
        opLocationElasticDTO.setName(opLocationEntity.getName());
        opLocationElasticDTO.setOperatorId(opLocationEntity.getOperatorId());
        opLocationElasticDTO.setOwnerId(opLocationEntity.getOwnerId());
        opLocationElasticDTO.setPostalCode(opLocationEntity.getPostalCode());
        opLocationElasticDTO.setStatus(opLocationEntity.getStatus());
        opLocationElasticDTO.setSubOperatorId(opLocationEntity.getSubOperatorId());
        opLocationElasticDTO.setTimeZone(opLocationEntity.getTimeZone());
        opLocationElasticDTO.setType(opLocationEntity.getType());
        opLocationElasticDTO.setUpdatedAt(opLocationEntity.getUpdatedAt());
        opLocationElasticDTO.setId(opLocationEntity.getId());
        opLocationElasticDTO.setZoneId(opLocationEntity.getZoneId());
        opLocationElasticDTO.setLocationType(opLocationEntity.getLocationType());
        opLocationElasticDTO.setTaxConfiguration(opLocationEntity.getTaxConfiguration());
        opLocationElasticDTO.setConditionsValue(opLocationEntity.getConditionsValue());
        opLocationElasticDTO.setHubjectCheck(opLocationEntity.getHubjectCheck());
        opLocationElasticDTO.setPayMethod(opLocationEntity.getPayMethod());
        opLocationElasticDTO.setPrepaymentAmountTier(opLocationEntity.getPrepaymentAmountTier());
        opLocationElasticDTO.setBusinessType(opLocationEntity.getBusinessType());
        return opLocationElasticDTO;
    }
}
