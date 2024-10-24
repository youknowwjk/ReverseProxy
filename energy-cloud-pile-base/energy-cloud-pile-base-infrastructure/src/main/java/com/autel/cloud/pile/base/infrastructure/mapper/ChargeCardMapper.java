package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.dto.CardOldDTO;
import com.autel.cloud.pile.base.dto.CardOptionsDTO;
import com.autel.cloud.pile.base.dto.ChargeCardBusinessPageDTO;
import com.autel.cloud.pile.base.dto.ChargeCardPageDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardEntity;
import com.autel.cloud.pile.base.vo.CardOptionsPageVO;
import com.autel.cloud.pile.base.vo.ChargeCardBusinessVO;
import com.autel.cloud.pile.base.vo.ChargeCardPageVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * @Author A22282
 * @Date 2022/5/9 17:17
 */
@Mapper
@Repository
public interface ChargeCardMapper extends BaseMapper<ChargeCardEntity> {
    IPage<ChargeCardBusinessVO> pagesForBusiness(ChargeCardBusinessPageDTO pages, @Param("params") ChargeCardBusinessPageDTO params);

    Integer updateBatchByCardNumber(List<ChargeCardEntity> datas);

    boolean saveBatch(@Param("insertList") List<CardOldDTO> insertList);

    Integer countAll();

    List<CardOldDTO> selectByBatch(@Param("page") int page, @Param("size") int size, @Param("cardNumbers") List<String> cardNumbers);

    IPage<ChargeCardPageVO> pagesV2(@Param("params") ChargeCardPageDTO params, @Param("list") List<Long> list);

    @Select("SELECT card_alias,card_brand FROM `tb_charge_card` WHERE card_number = #{cardNumber}")
    ChargeCardPageVO getCardInfoByNumber(String cardNumber);

    IPage<CardOptionsPageVO> cardOptionPageList(@Param("params") CardOptionsDTO params, @Param("keyWord") String keyWord, @Param("operatorId") Long operatorId);
}
