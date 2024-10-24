package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPileShareWhiteListEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

@Mapper
@Repository
public interface OpPileShareWhiteListMapper extends BaseMapper<OpPileShareWhiteListEntity> {
}
