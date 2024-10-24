package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.bo.OpLocationMapQueryBO;
import com.autel.cloud.pile.base.dto.OpLocationListDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.vo.EroamingPileVO;
import com.autel.cloud.pile.base.vo.GetSellerInfoAndLocationInfoVO;
import com.autel.cloud.pile.base.vo.LocationRoamingVO;
import com.autel.cloud.pile.base.vo.OpLocationMapQueryVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * <p>
 * 场站表 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Mapper
@Repository
public interface OpLocationMapper extends BaseMapper<OpLocationEntity> {
    /**
     *  根据名称查出id
     * @param name
     * @return
     */
    List<Long> selectByName(String name);

    /**
     *  根据id逻辑删除记录。将delete改为1
     * @param id
     */
    void deleteByLocationId(Long id);

    /**
     * app地图搜索场站
     *
     * @param opLocationMapQueryBO 检索对象
     * @return 场站数据
     */
    List<OpLocationMapQueryVO> mapQuery(OpLocationMapQueryBO opLocationMapQueryBO);
    /**
     * 根据商户id搜索场站
     *
     * @param operatorId 商户id
     * @return 场站数据
     */
    List<OpLocationMapQueryVO> selectByOperatorId(Long operatorId);

    String getOperatorIdBySn(String evseSn);

    List<OpLocationListDTO> getLocationByPileSnList(List<String> pileSnList);

    String getSellIdByPileSn(String pileSn);

    List<String> getPileSnBySellId(String sellId);

    @Select("SELECT zone_id FROM op_location WHERE id =" +
            "(SELECT location_id FROM op_location_evse WHERE evse_sn = #{evseSn})")
    String getZonIdByEvseSn(String evseSn);

    @Select("select ocpi_enabled from op_location where id = #{locationId} ")
    Boolean queryLocationOcpiEnabled(@Param("locationId") String locationId);

    @Select("select DISTINCT t1.ocpi_enabled from op_location t1 " +
            "join op_location_evse t2 on t1.id = t2.location_id " +
            "where t2.tariff_id = #{tariffId} ")
    List<Boolean> queryTariffOcpiEnabled(@Param("tariffId") String tariffId);

    GetSellerInfoAndLocationInfoVO getLocationInfo(@Param("s") String s);

    boolean insertBatch(@Param("insertLocationList") List<OpLocationEntity> insertLocationList);

    @Select("SELECT " +
            "t2.id evseId, " +
            "t1.id locationId, " +
            "t3.eroaming_enable ocpiEnabled, " +
            "t2.evse_sn evseSn  " +
            "FROM " +
            "op_location t1 " +
            "JOIN op_location_evse t2 ON t1.id = t2.location_id " +
            "join op_location_pile_evse t3 on SUBSTR(t2.evse_sn, 1, locate('_', t2.evse_sn) - 1) = t3.pile_sn " +
            "WHERE  " +
            "t1.deleted = 0 and t3.deleted = 0 and t2.deleted=0 and t2.evse_sn =#{evseSn} ")
    LocationRoamingVO getLocationRoamingByEvseSn(@Param("evseSn") String evseSn);

    @Select("select t1.id evseId, t2.id locationId, t2.operator_id operatorId from op_location_evse t1 " +
            "join op_location t2 on t1.location_id = t2.id " +
            "join op_location_pile_evse t3 on SUBSTR(t1.evse_sn, 1, locate('_', t1.evse_sn) - 1) = t3.pile_sn " +
            "where t3.eroaming_enable = 1 and t1.deleted = 0 and t3.deleted = 0 and t2.deleted=0 and t1.tariff_id =#{tariffId}")
    List<EroamingPileVO> getEroamingPileListByTariff(@Param("tariffId") Long tariffId);

    @Select("select pay_method from op_location where id=#{locationId}")
    Integer getPayMethod(@Param("locationId") Long locationId);

    @Select("select pay_method payMethod, prepayment_amount_tier prepaymentAmountTier  from op_location where id=#{locationId}")
    OpLocationEntity getLocationPrePayment(Long locationId);
}
