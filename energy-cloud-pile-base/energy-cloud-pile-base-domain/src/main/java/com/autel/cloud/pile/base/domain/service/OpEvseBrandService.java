package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.vo.OpEvseBrandVO;

import java.util.List;

public interface OpEvseBrandService {

    Result<List<OpEvseBrandVO>> list();
}
