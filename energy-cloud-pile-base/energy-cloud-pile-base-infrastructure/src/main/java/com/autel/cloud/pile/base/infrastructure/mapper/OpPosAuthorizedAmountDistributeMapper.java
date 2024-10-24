package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPosAuthorizedAmountDistributeEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * @Author A22599
 * @Date 2023/06/02
 * @Function 本地POS预授权金额修改工具设置 Mapper 接口
 */
@Mapper
@Repository
public interface OpPosAuthorizedAmountDistributeMapper extends BaseMapper<OpPosAuthorizedAmountDistributeEntity> {
}
