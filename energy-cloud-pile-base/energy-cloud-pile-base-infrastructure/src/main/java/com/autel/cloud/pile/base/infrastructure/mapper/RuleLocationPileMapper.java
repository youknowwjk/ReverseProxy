package com.autel.cloud.pile.base.infrastructure.mapper;


import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleLocationPileEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
* @author temp
* @description 针对表【tb_rule_location_pile(进场控制规则与桩关系表)】的数据库操作Mapper
* @createDate 2022-07-07 21:19:47
* @Entity generator.domain.RuleLocationEvseEntity
*/
@Mapper
@Repository
public interface RuleLocationPileMapper extends BaseMapper<RuleLocationPileEntity> {

}




