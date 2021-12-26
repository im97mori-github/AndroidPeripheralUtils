package org.im97mori.ble.android.peripheral.ui.device.setting.fragment;

import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import io.reactivex.rxjava3.disposables.CompositeDisposable;

public abstract class BaseDeviceSettingFragment extends Fragment {

    protected final CompositeDisposable mDisposable = new CompositeDisposable();

    @Override
    public void onStop() {
        mDisposable.dispose();
        super.onStop();
    }

    @Nullable
    public abstract String getModuleDataJson();
}
