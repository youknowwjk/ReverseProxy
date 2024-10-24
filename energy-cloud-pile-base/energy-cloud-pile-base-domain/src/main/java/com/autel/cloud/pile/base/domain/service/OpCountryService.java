package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.OpCountryDTO;
import com.autel.cloud.pile.base.dto.UserAlpha2CodeAndLanguage;
import com.autel.cloud.pile.base.vo.OpCountryInfoVO;
import com.autel.cloud.pile.base.vo.OpCountryVO;

import java.util.List;
import java.util.Map;

public interface OpCountryService {

    List<OpCountryVO> list();

    List<OpCountryVO> getAll(String language);

    List<OpCountryInfoVO> getCountryInfo(String language, List<String> abbrCodeList);

    List<OpCountryVO> getCountryInfoByLanguageAndAlpha2Code(UserAlpha2CodeAndLanguage userAlpha2CodeAndLanguage);

    public List<OpCountryVO> saveCountryCurrencyRelation(List<OpCountryDTO> requestDTO);

    List<OpCountryVO> getAllCode(String language);

    Boolean syncCountryCurrencyId();

    List<OpCountryVO> getCountryInfoListByCountryAbbreList(List<String> countryAbbreList);

    Map<String,String> getCountryMap(List<String> countryAbbreList);
}
