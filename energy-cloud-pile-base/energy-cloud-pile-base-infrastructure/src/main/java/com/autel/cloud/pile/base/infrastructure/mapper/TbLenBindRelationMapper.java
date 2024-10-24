package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.feign.dto.LastExpireBindRelation;
import com.autel.cloud.pile.base.infrastructure.feign.dto.LastExpireTimeDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity;
import com.autel.cloud.pile.base.vo.ChargeCardPageVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import lombok.Data;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;

import static org.springframework.transaction.annotation.Propagation.NOT_SUPPORTED;

@Repository
@Mapper
public interface TbLenBindRelationMapper extends BaseMapper<TbLenBindRelationEntity> {

    /**
     * 更新状态
     *
     * @return 更新的行数
     */
    @Transactional(propagation = NOT_SUPPORTED)
    int updateStatus(@Param("currentMills") Long currentMills);


    List<LastExpireBindRelation> findLastExpireBindRelation(@Param("pileSns") Collection<String> pileSns, @Param("serviceId") String serviceId, @Param("tenantId") Long tenantId);

    /**
     * 统一桩管理页 列表查询使用 其他地方不要用
     * <p>
     * 因为这里没有区分权益ID
     *
     * @param pileSns
     * @param tenantId
     * @return
     */
    List<LastExpireTimeDTO> findLastExpireTime(@Param("pileSns") Collection<String> pileSns, @Param("tenantId") Long tenantId);
}
