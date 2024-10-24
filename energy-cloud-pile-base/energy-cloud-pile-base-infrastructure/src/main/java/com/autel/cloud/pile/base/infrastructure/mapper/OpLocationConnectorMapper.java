package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.dto.OpLocationConnectorDTO;
import com.autel.cloud.pile.base.dto.OpLocationConnectorPagingDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.pile.base.vo.OcppLocationEVSETimeZoneVO;
import com.autel.cloud.pile.base.vo.OcppLocationEVSEVO;
import com.autel.cloud.pile.base.vo.OpLocationConnectorPagingVo;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * <p>
 * 充电设备连接器 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Mapper
@Repository
public interface OpLocationConnectorMapper extends BaseMapper<OpLocationConnectorEntity> {

    int batchSave(@Param("insertList")List<OpLocationConnectorEntity> insertList);

    /**
     *  根据locationEvseId删除充电设备连接器表记录（逻辑删除）
     * @param id
     */
    Boolean deleteByLocationEvseId(Long id);

    /**
     *  根据locationEvseId删除充电设备连接器表记录（逻辑删除）
     * @param id
     */
    Boolean deleteByLocationEvseIdList(List<Long> id);

    /**
     *  更新记录
     * @param opLocationConnectorEntity
     * @return
     */
    Boolean updateByLocationEvseId(OpLocationConnectorEntity opLocationConnectorEntity);

    /**
     * 分页获取连接器
     */
    Page<OpLocationConnectorPagingDTO> paging(Page<OpLocationConnectorPagingDTO> page, @Param("opLocationConnectorPagingVo")OpLocationConnectorPagingVo opLocationConnectorPagingVo);


    /**
     * 基于充电桩查询相关数据
     * @param evseSn
     * @return
     */
    OcppLocationEVSEVO getLocationEvseVOBySnAndGunNo(String evseSn);

    /**
     * 基于充电桩查询场站信息
     * @param pileSn
     * @return
     */
    OcppLocationEVSEVO getLocationByPileSn(String pileSn);

    /**
     * 根据充电设备Sn查询（id不是sn，是locationEvseId)
     *
     * @param id
     * @return
     */
    List<OpLocationConnectorDTO> selectByLocationEvseSn(Long id);

    @Select("select * from op_location_connector where deleted = 0 and location_evse_id=#{evseId}")
    List<OpLocationConnectorEntity> getConnectorListByEvseId(@Param("evseId") Long evseId);

    @Select("select * from op_location_connector a join op_location_evse b on b.id=a.location_evse_id where a.deleted = 0 and b.evse_sn like CONCAT('%',#{evseSn},'%')")
    List<OpLocationConnectorEntity> getConnectorListBysn(@Param("evseSn") String evseSn);

    List<OcppLocationEVSETimeZoneVO> getOcppLocationEVSETimeZoneVOList(@Param("evseSnList") List<String> evseSnList);
}
