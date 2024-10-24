package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.ChargePointNoticeEvent;
import com.autel.cloud.pile.base.domain.model.ChargePointDTO;
import com.autel.cloud.pile.base.dto.OpLocationConnectorDTO;
import com.autel.cloud.pile.base.dto.OpLocationEvseDTO;
import com.autel.cloud.pile.base.enums.BrandEnum;
import com.autel.cloud.pile.base.enums.MerchantChargePointRelationEnum;
import com.autel.cloud.pile.base.enums.RelationshipEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantRelationEntity;
import com.autel.cloud.pile.base.vo.*;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.mapstruct.*;
import org.mapstruct.factory.Mappers;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;

/**
 * @author A22136
 * @date 2022-03-02 15:42
 */
@Mapper
public interface ChargePointMerchantRelationTypeMapper {

    OpLocationEvseDTO entityMapOpLocationEvseDTO(ChargePointMerchantRelationEntity source);

    Page<ChargePointAssetVO> pageEntityList2AssetPageVO(Page<ChargePointMerchantRelationEntity> source);

    ChargePointMerchantRelationTypeMapper INSTANCE = Mappers.getMapper(ChargePointMerchantRelationTypeMapper.class);

    List<ChargePointMerchantRelationEntity> map(List<ChargePointDTO> source);

    List<ChargePointAssetVO> entityMapVO(List<ChargePointMerchantRelationEntity> source);

    ChargePointAssetVO entityMapVO(ChargePointMerchantRelationEntity source);

    ChargePointMerchantRelationEntity dto2Entity(ChargePointDTO source, @Context MerchantChargePointRelationEnum relationEnum);

    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    void updateChargePointMerchantRelationEntity(ChargePointDTO source, @MappingTarget ChargePointMerchantRelationEntity target);

    ChargePointNoticeEvent chargePointMerchantRelationEntityToChargePointNoticeEvent(ChargePointMerchantRelationEntity chargePointMerchantRelationEntity);

    ChargePointSourceVO entity2SourceVO(ChargePointMerchantRelationEntity source);

    List<ChargePointSourceVO> entities2SourcesVO(List<ChargePointMerchantRelationEntity> sources);

    ChargePointDetailVO entity2VO(ChargePointMerchantRelationEntity source);

    ChargePointRecordVO entity2RecordVO(ChargePointMerchantRelationEntity source);

    @AfterMapping
    default void afterMapping(@MappingTarget ChargePointNoticeEvent target, ChargePointMerchantRelationEntity source) {
        if (MerchantChargePointRelationEnum.MAINTENANCE.equals(MerchantChargePointRelationEnum.keyOf(source.getRelation()))) {
            target.setName_1(source.getName());
            target.setMaintenance(source.getMerchantId());
            target.setMaintenanceBindTime(source.getBindTime());
        }
        if (MerchantChargePointRelationEnum.OWNER.equals(MerchantChargePointRelationEnum.keyOf(source.getRelation()))) {
            target.setName_3(source.getName());
            target.setOwner(source.getMerchantId());
            target.setOwnerBindTime(source.getBindTime());
        }
        target.setThirdPart(!Long.valueOf(BrandEnum.AUTEL.getCode()).equals(source.getBrandId()));
        target.setSubscriptionStatus(source.getSubStatus());
    }


    @AfterMapping
    default void afterMapping(@MappingTarget ChargePointMerchantRelationEntity target, ChargePointDTO source, @Context MerchantChargePointRelationEnum relationEnum) {
        target.setRelation(relationEnum.getKey());
    }

    Page<ChargePointVO> pageEntityList2PageVO(Page<ChargePointMerchantRelationEntity> chargePointEntityPage);

    List<ChargePointVO> entityList2VO(List<ChargePointMerchantRelationEntity> chargePointEntityList);

    ChargePointVO entity2ChargePointVO(ChargePointMerchantRelationEntity source);

    @AfterMapping
    default void afterMapping(@MappingTarget ChargePointVO target, ChargePointMerchantRelationEntity source) {
//        if (Objects.nonNull(target.getSubscriptionStatus()) && target.getSubscriptionStatus() >= SubStatus.SOON_TO_EXPIRE.getStatus()) {
//            String zoneId = target.getZoneId();
//            String format = LocalDateTime.ofInstant(Instant.ofEpochMilli(target.getExpireTimestamp()),
//                    StringUtils.isBlank(zoneId) ? ZoneId.systemDefault() : ZoneId.of(zoneId)).format(ChargePointService.dateFormatter);
//            target.setExpireDate(format);
//        }
        if (Objects.nonNull(target) && Objects.nonNull(source)) {
            if (MerchantChargePointRelationEnum.MAINTENANCE.equals(MerchantChargePointRelationEnum.keyOf(source.getRelation()))) {
                target.setRelationship(RelationshipEnum.MAINTENANCE);
            }
            if (MerchantChargePointRelationEnum.OWNER.equals(MerchantChargePointRelationEnum.keyOf(source.getRelation()))) {
                target.setRelationship(RelationshipEnum.OWNER);
            }
            target.setThirdPart(!source.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode())));
            if (CollectionUtils.isEmpty(source.getConnectors())) {
                target.setCountGun(0);
            } else {
                target.setCountGun(source.getConnectors().size());
            }
        }
    }

    OpLocationConnectorDTO toConnectorDTO(Connector connector);
}
