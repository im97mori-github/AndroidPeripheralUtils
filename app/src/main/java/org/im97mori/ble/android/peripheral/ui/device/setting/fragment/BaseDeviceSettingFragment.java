package org.im97mori.ble.android.peripheral.ui.device.setting.fragment;

import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import io.reactivex.rxjava3.disposables.CompositeDisposable;

public abstract class BaseDeviceSettingFragment extends Fragment {

    protected final CompositeDisposable mDisposable = new CompositeDisposable();

    public BaseDeviceSettingFragment(int contentLayoutId) {
        super(contentLayoutId);
    }

    @Override
    public void onStop() {
        mDisposable.clear();
        super.onStop();
    }

    @Nullable
    public abstract String getModuleDataString();
}
