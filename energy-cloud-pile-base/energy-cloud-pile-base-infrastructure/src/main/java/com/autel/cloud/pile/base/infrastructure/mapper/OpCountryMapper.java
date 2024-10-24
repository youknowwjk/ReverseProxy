package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpCountryEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * 国家区号 Mapper 接口
 * </p>
 *
 * @author A22327
 * @since 2022-04-14
 */
@Repository
@Mapper
public interface OpCountryMapper extends BaseMapper<OpCountryEntity> {

}
