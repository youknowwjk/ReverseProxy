package com.autel.cloud.pile.base.domain.convert;


import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.dto.OpLocationDTO;
import com.autel.cloud.pile.base.dto.OpLocationMenuDTO;
import com.autel.cloud.pile.base.dto.tax.TaxDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationOperationEntity;
import com.autel.cloud.pile.bill.enums.PayMethodEnum;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.springframework.beans.BeanUtils;
import org.springframework.data.elasticsearch.core.geo.GeoPoint;

public class OpLocationConvert {

    private OpLocationConvert() {

    }

    /**
     * 转换opLocationEntity
     *
     * @param opLocationDTO
     * @return
     */
    public static OpLocationEntity opLocationEntity(OpLocationDTO opLocationDTO) {
        if (opLocationDTO == null) {
            return null;
        }
        OpLocationEntity opLocationEntity = new OpLocationEntity();
        opLocationEntity.setId(opLocationDTO.getId());
        opLocationEntity.setDeleted(opLocationDTO.getDeleted());
        opLocationEntity.setStatus(opLocationDTO.getStatus());
        opLocationEntity.setType(opLocationDTO.getType());
        opLocationEntity.setName(opLocationDTO.getName());
        opLocationEntity.setAddress(opLocationDTO.getAddress());
        opLocationEntity.setCity(opLocationDTO.getCity());
        opLocationEntity.setPostalCode(opLocationDTO.getPostalCode());
        opLocationEntity.setCountry(opLocationDTO.getCountry());
        opLocationEntity.setProvince(opLocationDTO.getProvince());
        opLocationEntity.setLatitude(opLocationDTO.getLatitude());
        opLocationEntity.setLongitude(opLocationDTO.getLongitude());
        opLocationEntity.setSubOperatorId(opLocationDTO.getSubOperatorId());
        opLocationEntity.setOwnerId(opLocationDTO.getOwnerId());
        opLocationEntity.setTimeZone(opLocationDTO.getTimeZone());
        opLocationEntity.setChargingWhenClosed(opLocationDTO.getChargingWhenClosed());
        opLocationEntity.setZoneId(opLocationDTO.getZoneId());
        opLocationEntity.setLocationType(opLocationDTO.getLocationType());
        opLocationEntity.setPayMethod(opLocationDTO.getPayMethod() == null ? PayMethodEnum.PAY_LATER.getValue() :opLocationDTO.getPayMethod());

        TaxDTO taxDTO = opLocationDTO.getTaxDTO();
        if (taxDTO != null) {
            Integer setLocalTax = taxDTO.getSetLocalTax();
            if (!Integer.valueOf(1).equals(setLocalTax)) {
                taxDTO.setSetLocalTax(0);
                taxDTO.setLocalTaxDTO(null);
            }
            opLocationEntity.setTaxConfiguration(JSON.toJSONString(taxDTO));
        }

        opLocationEntity.setHubjectCheck(opLocationDTO.getHubjectCheck());
        opLocationEntity.setConditionsValue(opLocationDTO.getConditionsValue());
        opLocationEntity.setUpdatedAt(System.currentTimeMillis());
        opLocationEntity.setBusinessType(opLocationDTO.getBusinessType());
        opLocationEntity.setHouseNumber(opLocationDTO.getHouseNumber());
        opLocationEntity.setOperatorId(opLocationDTO.getOperatorId());
        opLocationEntity.setReservationEnabled(opLocationDTO.getReservationEnabled());
        return opLocationEntity;
    }

    /**
     * 转换OpLocationMenuDTO
     *
     * @param opLocationElasticDTO
     * @return
     */
    public static OpLocationMenuDTO toOpLocationMenuDTO(OpLocationElasticDTO opLocationElasticDTO) {
        OpLocationMenuDTO opLocationMenuDTO = new OpLocationMenuDTO();
        opLocationMenuDTO.setId(opLocationElasticDTO.getId());
        opLocationMenuDTO.setName(opLocationElasticDTO.getName());
        return opLocationMenuDTO;
    }

    public static OpLocationMenuDTO toOpLocationMenuDTO(OpLocationEntity locationEntity) {
        OpLocationMenuDTO opLocationMenuDTO = new OpLocationMenuDTO();
        opLocationMenuDTO.setId(locationEntity.getId());
        opLocationMenuDTO.setName(locationEntity.getName());
        opLocationMenuDTO.setDeleted(locationEntity.getDeleted());
        opLocationMenuDTO.setCreatedAt(locationEntity.getCreatedAt());
        return opLocationMenuDTO;
    }

    /**
     * 转换OpLocationDTO
     *
     * @param opLocationDTO
     * @param opLocationEntity
     * @return
     */
    public static OpLocationDTO opLocationDTO(OpLocationDTO opLocationDTO, OpLocationEntity opLocationEntity) {
        toOpLocation(opLocationDTO, opLocationEntity);
        opLocationDTO.setLocationId(opLocationEntity.getId());
        opLocationDTO.setPayMethod(opLocationEntity.getPayMethod());

        if(PayMethodEnum.ADVANCE_PAYMENT.getValue().equals(opLocationEntity.getPayMethod())){
            opLocationDTO.setPrepaymentAmountTier(opLocationEntity.getPrepaymentAmountTier());
        }else{
            opLocationDTO.setPrepaymentAmountTier(null);
        }

        return opLocationDTO;
    }

    private static void toOpLocation(OpLocationDTO opLocationDTO, OpLocationEntity opLocationEntity) {
        opLocationDTO.setStatus(opLocationEntity.getStatus());
        opLocationDTO.setName(opLocationEntity.getName());
        opLocationDTO.setAddress(opLocationEntity.getAddress());
        opLocationDTO.setPostalCode(opLocationEntity.getPostalCode());
        opLocationDTO.setCountry(opLocationEntity.getCountry());
        opLocationDTO.setProvince(opLocationEntity.getProvince());
        opLocationDTO.setLatitude(opLocationEntity.getLatitude());
        opLocationDTO.setLongitude(opLocationEntity.getLongitude());
        opLocationDTO.setTimeZone(opLocationEntity.getTimeZone());
        opLocationDTO.setZoneId(opLocationEntity.getZoneId());
        opLocationDTO.setOwnerId(opLocationEntity.getOwnerId());
        opLocationDTO.setDeleted(opLocationEntity.getDeleted());
        opLocationDTO.setType(opLocationEntity.getType());
        opLocationDTO.setCity(opLocationEntity.getCity());
        opLocationDTO.setOperatorId(opLocationEntity.getOperatorId());
        opLocationDTO.setSubOperatorId(opLocationEntity.getSubOperatorId());
        opLocationDTO.setChargingWhenClosed(opLocationEntity.getChargingWhenClosed());
        opLocationDTO.setHouseNumber(opLocationEntity.getHouseNumber());
        opLocationDTO.setHubjectCheck(opLocationEntity.getHubjectCheck());
        opLocationDTO.setConditionsValue(opLocationEntity.getConditionsValue());
        opLocationDTO.setReservationEnabled(opLocationEntity.getReservationEnabled());
        opLocationDTO.setBusinessType(opLocationEntity.getBusinessType());
    }

    public static OpLocationDTO excelToOpLocationDTO(Row row) {
        OpLocationDTO opLocationDTO = new OpLocationDTO();
        Cell name = row.getCell(0);
        opLocationDTO.setName(name.toString());
        opLocationDTO.setDeleted(Integer.parseInt(row.getCell(1).toString()));
        opLocationDTO.setStatus(Integer.parseInt(row.getCell(2).toString()));
        opLocationDTO.setType(row.getCell(3).toString());
        opLocationDTO.setAddress(row.getCell(4).toString());
        opLocationDTO.setCity(row.getCell(5).toString());
        opLocationDTO.setPostalCode(row.getCell(6).toString());
        opLocationDTO.setCountry(row.getCell(7).toString());
        opLocationDTO.setLatitude(row.getCell(8).toString());
        opLocationDTO.setLongitude(row.getCell(2).toString());
        opLocationDTO.setOperatorId(Long.getLong(row.getCell(2).toString()));
        opLocationDTO.setSubOperatorId(Long.getLong(row.getCell(2).toString()));
        opLocationDTO.setOwnerId(Long.getLong(row.getCell(2).toString()));
        opLocationDTO.setTimeZone(row.getCell(2).toString());
        opLocationDTO.setChargingWhenClosed(Integer.parseInt(row.getCell(2).toString()));
        opLocationDTO.setOperationType(row.getCell(2).toString());
        opLocationDTO.setState(Integer.parseInt(row.getCell(2).toString()));
        opLocationDTO.setServiceTel(row.getCell(2).toString());
        opLocationDTO.setAnnouncement(row.getCell(2).toString());
        opLocationDTO.setOpenType(Integer.parseInt(row.getCell(2).toString()));
        opLocationDTO.setAppShow(Boolean.valueOf(row.getCell(2).toString()));
        return opLocationDTO;
    }

    /**
     * 转换
     *
     * @param opLocationElasticDTO
     * @return
     */
    public static OpLocationDTO toOpLocationDTO(OpLocationElasticDTO opLocationElasticDTO) {
        OpLocationDTO opLocationDTO = new OpLocationDTO();
        if (opLocationElasticDTO != null) {
            BeanUtils.copyProperties(opLocationElasticDTO, opLocationDTO);
            String taxConfiguration = opLocationElasticDTO.getTaxConfiguration();
            if (StringUtils.isNotBlank(taxConfiguration)) {
                opLocationDTO.setTaxDTO(JSON.parseObject(taxConfiguration, TaxDTO.class));
            }
        }
        return opLocationDTO;
    }

    /**
     *
     * @param target
     * @param source
     * @param operation
     */
    public static void toOpLocationElasticDTO(OpLocationElasticDTO target, OpLocationEntity source, OpLocationOperationEntity operation) {
        target.setId(source.getId());
        target.setCreatedAt(source.getCreatedAt());
        target.setUpdatedAt(source.getUpdatedAt());
        target.setState(source.getStatus());
        target.setType(source.getType());
        target.setName(source.getName());
        target.setAddress(source.getAddress());
        target.setCity(source.getCity());
        target.setPostalCode(source.getPostalCode());
        target.setCountry(source.getCountry());
        target.setProvince(source.getProvince());
        target.setLatitude(source.getLatitude());
        target.setLongitude(source.getLongitude());
        GeoPoint location = new GeoPoint(Double.parseDouble(source.getLatitude()), Double.parseDouble(source.getLongitude()));
        target.setLocation(location);
        target.setOperatorId(source.getOperatorId());
        target.setSubOperatorId(source.getSubOperatorId());
        target.setOwnerId(source.getOwnerId());
        target.setZoneId(source.getZoneId());
        target.setChargingWhenClosed(source.getChargingWhenClosed());
        target.setOpenType(operation.getOpenType());
        target.setOperationType(operation.getOperationType());
        target.setState(operation.getState());
        target.setServiceTel(operation.getServiceTel());
        target.setAnnouncement(operation.getAnnouncement());
        target.setAnnouncementAt(operation.getAnnouncementAt());
        target.setAppShow(operation.getAppShow());
        target.setBillingRule(operation.getBillingRule());
        target.setOperationDate(operation.getOperationDate());
        target.setReservationEnabled(source.getReservationEnabled());
        target.setHouseNumber(source.getHouseNumber());
        target.setHubjectCheck(source.getHubjectCheck());
        target.setLocationType(source.getLocationType());
        target.setTaxConfiguration(source.getTaxConfiguration());
        target.setPayMethod(source.getPayMethod());
        target.setPrepaymentAmountTier(source.getPrepaymentAmountTier());
        target.setBusinessType(source.getBusinessType());
        target.setTimeZone(source.getTimeZone());
    }
}
