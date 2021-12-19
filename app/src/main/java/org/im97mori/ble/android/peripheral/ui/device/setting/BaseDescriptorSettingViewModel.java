package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;

import org.im97mori.ble.DescriptorData;

import java.util.Optional;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;

public abstract class BaseDescriptorSettingViewModel extends BaseSettingViewModel {

    protected DescriptorData mDescriptorData;

    @NonNull
    public abstract Completable setup(@NonNull Intent intent);

    @NonNull
    public abstract Single<Optional<Intent>> save();

}