package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.dto.HubPileDTO;
import com.autel.cloud.pile.base.dto.OpLocationMapQueryDTO;
import com.autel.cloud.pile.base.dto.RuleSitePageDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.vo.OpLocationEvseMapQueryVO;
import com.autel.cloud.pile.base.vo.PileBindTimeVO;
import com.autel.cloud.pile.base.vo.PileDetailVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * <p>
 * 充电设备 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Mapper
@Repository
public interface OpLocationEvseMapper extends BaseMapper<OpLocationEvseEntity> {

    int batchSave(@Param("insertList")List<OpLocationEvseEntity> insertList);
    /**
     * 根据id逻辑删除充电设备
     *
     * @param id
     */
    void deleteByEvseId(Long id);

    /**
     * 根据桩SN码删除充电设备
     *
     * @param pileSN 桩SN码
     */
    void deleteByPileSN(String pileSN);

    /**
     * 查找枪设备
     *
     * @param opLocationMapQueryDTO app地图搜索场站
     * @return 枪设备集合
     */
    List<OpLocationEvseMapQueryVO> mapQuery(OpLocationMapQueryDTO opLocationMapQueryDTO);

    @Select({"<script>" +
            " select t1.* from op_location_evse t1 " +
            " join op_location_pile_evse t2 on SUBSTR(t1.evse_sn, 1, locate('_', t1.evse_sn) - 1) = t2.pile_sn " +
            " where t1.deleted = 0 and t2.deleted = 0 and t2.eroaming_enable = 1 and t2.location_id=#{locationId} " +
            "<if test='evseUid !=null'> and t1.id = #{evseUid} </if>" +
            "</script>"
    })
    List<OpLocationEvseEntity> getEvseListByLocationId(@Param("locationId") Long locationId, @Param("evseUid") String evseUid);

    IPage<PileDetailVO> selectPageByKeyword(RuleSitePageDTO page, @Param("params") RuleSitePageDTO params);

    /**
     * 根据 pileSnList 查询充电桩信息
     * @param pileSnList
     * @return
     */
    List<HubPileDTO> getPileInfoByPileSn(@Param("pileSnList") List<String> pileSnList);

    @Select({"<script>" +
            " SELECT distinct tariff_id FROM op_location_evse WHERE deleted = 0 and tariff_id is not null and evse_sn IN " +
            " <foreach  item='item'  collection='evseSnList' open='(' separator=',' close=')'> #{item}</foreach>" +
            "</script>"})
    List<String> getTariffIdListByEvseSn(@Param("evseSnList") List<String> evseSnList);

    @Select("select * from op_location_evse where id= #{evseUid}")
    List<OpLocationEvseEntity> getEvseListByEvseId(@Param("evseUid") String evseUid);
    
    List<PileBindTimeVO> batchQueryPileBindTime(@Param("snList") List<String> snList);

    @Select({"<script>" +
            " SELECT tariff_id tariffId, evse_sn  evseSn FROM op_location_evse WHERE deleted = 0 and evse_sn IN " +
            " <foreach  item='item'  collection='evseSnList' open='(' separator=',' close=')'> #{item}</foreach>" +
            "</script>"})
    List<OpLocationEvseEntity> queryTariffByEvseSnList(@Param("evseSnList") List<String> evseSnList);
}
