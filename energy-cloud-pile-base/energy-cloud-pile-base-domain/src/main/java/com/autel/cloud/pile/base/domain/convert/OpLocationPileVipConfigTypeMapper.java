package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.VipPolicy;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileVipConfigEntity;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import org.mapstruct.AfterMapping;
import org.mapstruct.Mapper;
import org.mapstruct.MappingTarget;
import org.mapstruct.factory.Mappers;

import java.util.List;
import java.util.Objects;

import static com.autel.cloud.pile.base.domain.repository.OpLocationPileVipConfigRepository.LONG_TERM_EFFECTIVE_DATE;
import static com.autel.cloud.pile.base.domain.repository.OpLocationPileVipConfigRepository.LONG_TERM_EXPIRE_DATE;

/**
 * @author A22136
 * @date 2022-03-02 15:42
 */
@Mapper
public interface OpLocationPileVipConfigTypeMapper {


    OpLocationPileVipConfigTypeMapper INSTANCE = Mappers.getMapper(OpLocationPileVipConfigTypeMapper.class);

    OpLocationPileVipConfigEntity map(VipPolicy opLocationPileVipConfigDTO);

    VipPolicy map(OpLocationPileVipConfigEntity entity);

    List<OpLocationPileVipConfigEntity> map(List<VipPolicy> opLocationPileVipConfigDTO);

    List<VipPolicy> mapVO(List<OpLocationPileVipConfigEntity> opLocationPileVipConfigDTO);


    @AfterMapping
    default void afterMapping(@MappingTarget OpLocationPileVipConfigEntity target, VipPolicy source) {
        if (Objects.isNull(target.getId())) {
            target.setId(IdWorker.getId());
        }
        if (source.isLongTerm()) {
            target.setEffectiveDate(LONG_TERM_EFFECTIVE_DATE);
            target.setExpirationDate(LONG_TERM_EXPIRE_DATE);
        }
    }
}
