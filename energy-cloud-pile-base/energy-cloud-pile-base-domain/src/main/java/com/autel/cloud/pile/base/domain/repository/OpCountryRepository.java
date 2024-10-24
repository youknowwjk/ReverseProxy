package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.OpCountryDTO;
import com.autel.cloud.pile.base.dto.UpdatePowerDTO;
import com.autel.cloud.pile.base.dto.UserAlpha2CodeAndLanguage;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpCountryEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface OpCountryRepository extends IService<OpCountryEntity> {
    /**
     * 国家简写转成全称
     *
     * @param simpleCode 国家简写
     * @return 国家全称
     */
    String simPleCode2Name(String simpleCode);

    /**
     * 根据国家缩写获取国家信息
     *
     * @param countryAbbre 国家简写
     * @return 国家全称
     */
    List<OpCountryEntity> getCountryInfoByCountryAbbre(String countryAbbre);

    /**
     * @param userAlpha2CodeAndLanguage
     * @return
     * @function 根据国家缩写和语言获取国家信息
     */
    List<OpCountryEntity> getCountryInfoByLanguageAndAlpha2Code(UserAlpha2CodeAndLanguage userAlpha2CodeAndLanguage);

    /**
     * 根据国家缩写获取国家信息
     *
     * @param requestDTO @return 国家全称
     * @return
     */
    List<OpCountryEntity> saveCountryCurrencyRelation(List<OpCountryDTO> requestDTO);

    Boolean syncCountryCurrencyId();

    List<OpCountryEntity> getCountryInfoListByCountryAbbreList(List<String> countryAbbreList);

    Boolean updatePower(UpdatePowerDTO updatePowerDTO);
}
