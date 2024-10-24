package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseBrandModelEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * 桩品牌型号 Mapper 接口
 * </p>
 *
 * @author A22587
 * @since 2022-09-13
 */

@Mapper
@Repository
public interface OpEvseBrandModelMapper extends BaseMapper<OpEvseBrandModelEntity> {

    Boolean deleteById(Long id);

}
