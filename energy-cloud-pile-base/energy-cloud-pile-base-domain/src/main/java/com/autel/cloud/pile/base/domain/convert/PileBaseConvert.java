package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.springframework.data.elasticsearch.core.geo.GeoPoint;
import org.springframework.util.StringUtils;

/**
 * @Author A22282
 * @Date 2024/4/30 14:41
 */
@Mapper(componentModel = "spring")
public interface PileBaseConvert {

    void toLocationElasticDto(@MappingTarget OpLocationElasticDTO target, OpLocationOperationEntity source);


    void toLocationElasticDto(@MappingTarget OpLocationElasticDTO target, OpLocationEntity source);

    void toPileEvseElasticDto(@MappingTarget OpLocationPileEvseElasticDTO target, OpLocationPileEvseEntity source);

    void toPileEvseElasticDto(@MappingTarget OpLocationPileEvseElasticDTO target, OpLocationEvseEntity source);

    @Mapping(target = "locationName", source = "name")
    void toPileEvseElasticDto(@MappingTarget OpLocationPileEvseElasticDTO target, OpLocationEntity source);

    @Mapping(ignore = true, source = "tariffId", target = "tariffId")
    void toPileEvseElasticDto(@MappingTarget OpLocationPileEvseElasticDTO target, OpLocationConnectorEntity source);

    @Mapping(ignore = true, source = "state", target = "state")
    void toEvseElasticDto(@MappingTarget OpLocationEvseElasticDTO target, OpLocationEvseEntity source);

    @Mapping(target = "pileName", source = "name")
    void toEvseElasticDto(@MappingTarget OpLocationEvseElasticDTO target, OpLocationPileEvseEntity source);

    @Mapping(target = "locationName", source = "name")
    @Mapping(target = "locationId", source = "id")
    void toEvseElasticDto(@MappingTarget OpLocationEvseElasticDTO target, OpLocationEntity source);

    @Mapping(ignore = true, source = "tariffId", target = "tariffId")
    void toEvseElasticDto(@MappingTarget OpLocationEvseElasticDTO target, OpLocationConnectorEntity source);

    /**
     * 注意如有相同字段，小心覆盖！
     * 注意如有相同字段，小心覆盖！
     * 注意如有相同字段，小心覆盖！
     *
     * @param target
     * @param source
     * @param operation
     * @return
     */
    default OpLocationElasticDTO handleRelation(OpLocationElasticDTO target, OpLocationEntity source, OpLocationOperationEntity operation) {
        this.toLocationElasticDto(target, operation);
        this.toLocationElasticDto(target, source);
        GeoPoint location = new GeoPoint(Double.parseDouble(source.getLatitude()), Double.parseDouble(source.getLongitude()));
        target.setLocation(location);
        return target;
    }

    /**
     * 注意如有相同字段，小心覆盖！
     * 注意如有相同字段，小心覆盖！
     * 注意如有相同字段，小心覆盖！
     *
     * @param target
     * @param pileSource
     * @param evseSource
     * @param locationSource
     * @param connectorSource
     * @return
     */
    default OpLocationPileEvseElasticDTO handleRelation(OpLocationPileEvseElasticDTO target, OpLocationPileEvseEntity pileSource, OpLocationEvseEntity evseSource, OpLocationEntity locationSource, OpLocationConnectorEntity connectorSource) {
        this.toPileEvseElasticDto(target, evseSource);
        this.toPileEvseElasticDto(target, locationSource);
        this.toPileEvseElasticDto(target, connectorSource);
        this.toPileEvseElasticDto(target, pileSource);
        return target;
    }

    /**
     * 注意如有相同字段，小心覆盖！
     * 注意如有相同字段，小心覆盖！
     * 注意如有相同字段，小心覆盖！
     *
     * @param target
     * @param evseSource
     * @param pileSource
     * @param locationSource
     * @param connectorSource
     * @return
     */
    default OpLocationEvseElasticDTO handleRelation(OpLocationEvseElasticDTO target, OpLocationEvseEntity evseSource, OpLocationPileEvseEntity pileSource, OpLocationEntity locationSource, OpLocationConnectorEntity connectorSource) {
        this.toEvseElasticDto(target, pileSource);
        this.toEvseElasticDto(target, locationSource);
        this.toEvseElasticDto(target, connectorSource);
        this.toEvseElasticDto(target, evseSource);
        String state = target.getState();
        if (!StringUtils.hasText(state)) {
            state = EvseDeviceStatusEnum.DEFAULT.getName();
        }
        target.setState(state);
        return target;
    }
}
